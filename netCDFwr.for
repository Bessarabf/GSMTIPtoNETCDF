      subroutine netCDFWriteGrid(ncid,gas,ids,its,nh,iit,rads,file_name,
     #                           time_id)
     
      ! Convert 3D data to proper netCDF format

      !      implicit none
      include 'netcdf.inc'

      ! error status return
      integer  iret,iit,nh,its,ids

      ! netCDF id
      integer  ncid

      ! dimension ids
      integer  lon_dim,lat_dim,p_dim,time_dim,p_len

      ! dimension lengths
      integer,parameter :: n_time =1
      integer,parameter :: time_len=NF_UNLIMITED

      ! number of considered variables
      character *100 file_name

      ! variable ids
      integer  lon_id,lat_id,p_id,time_id
      
      ! rank (number of dimensions) for each variable
      integer,parameter :: lon_rank =1
      integer,parameter :: lat_rank =1
      integer,parameter :: p_rank   =1
      integer,parameter :: time_rank=1
     
      ! variable shapes
      integer  lon_dims(lon_rank)
      integer  lat_dims(lat_rank)
      integer  p_dims(p_rank)
      integer  time_dims(time_rank)

      real*8  doubleval(1)
      
      ! data variables
      REAL(8), DIMENSION(:), allocatable :: lon  
      REAL(8), DIMENSION(:), allocatable :: lat
      REAL(8), DIMENSION(:), allocatable :: p
      
      ! starts and counts for array sections of record variables
      integer  time_start(time_rank),time_count(time_rank)
      
      ! local variables
      integer i,j,ik,status,it,k,is,iy
      character*100 out_file_name,in_file_name
      integer unlimdimid,irec

      ! varibles 
	real gas(ids,its,nh,n_time),rads(nh)
	real*8 ytime(n_time),dtime(12,2),scale,start_ytime,day
	real*8 xtime
      lon_len=ids
      lat_len=its
      p_len=nh
      
      allocate (lon(lon_len),lat(lat_len),p(p_len))
      
      data dtime(:,1) /31.,28.,31.,30.,31.,30.,31.,31.,30.,31.,30.,31./
      data dtime(:,2) /31.,29.,31.,30.,31.,30.,31.,31.,30.,31.,30.,31./
!!!!!!!!!!!!!!!!!!!!!!!!!	
      p=dble(rads*1.e-5)
      
!!!!!!!!!!!!!!!!!!!!!!!!!
! ----------- Time definition      
      do j=1,n_time
         ytime(j)=j*1.0  
      enddo
! ----------- Grid definition
      do i= 1,lon_len
          lon(i)=(i-1)*5.
      enddo

      do i= 1,lat_len
          lat(i)=90. - (i-1)*5.
      enddo

      ! enter define mode
      ! define dimensions
      iret=nf_def_dim(ncid,'lon',lon_len,lon_dim)
      call check_err(iret, 1)

	iret=nf_def_dim(ncid,'lat',lat_len,lat_dim)
      call check_err(iret, 2)

	iret=nf_def_dim(ncid,'plev',p_len,p_dim)
      call check_err(iret, 3)

	iret=nf_def_dim(ncid,'time',NF_UNLIMITED,time_dim)
      call check_err(iret, 4)

      ! define variables
      lon_dims(1)=lon_dim
      write(*,*) lon_dims
      iret=nf_def_var(ncid,'lon',NF_DOUBLE,lon_rank,lon_dims,lon_id)
      call check_err(iret, 5)

        !write(*,*) 'point 1.2'
      lat_dims(1)=lat_dim
      iret=nf_def_var(ncid,'lat',NF_DOUBLE,lat_rank,lat_dims,lat_id)
      call check_err(iret, 6)
      
      p_dims(1)=p_dim      
      iret=nf_def_var(ncid,'plev',NF_DOUBLE,p_rank,p_dims,p_id)
      call check_err(iret, 7)

      time_dims(1)=time_dim
      iret=nf_def_var(ncid,'time',NF_DOUBLE,time_rank,time_dims,time_id)
      call check_err(iret, 8)

 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!       

      ! assign attributes
      iret=nf_put_att_text(ncid,lon_id,'axis',1,'X')
      call check_err(iret, 10)
      iret=nf_put_att_text(ncid,lon_id,'long_name',9,'Longitude')
      call check_err(iret, 11)
      iret=nf_put_att_text(ncid,lon_id,'standard_name',9,'longitude')
      call check_err(iret, 12)
      iret=nf_put_att_text(ncid,lon_id,'units',12,'degrees_east')
      call check_err(iret, 13)

      iret=nf_put_att_text(ncid,lat_id,'axis',1,'Y')
      call check_err(iret, 14)
      iret=nf_put_att_text(ncid,lat_id,'long_name',8,'Latitude')
      call check_err(iret, 15)
      iret=nf_put_att_text(ncid,lat_id,'standard_name',8,'latitude')
      call check_err(iret, 16)
      iret=nf_put_att_text(ncid,lat_id,'units',13,'degrees_north')
      call check_err(iret, 17)

      iret=nf_put_att_text(ncid,p_id,'axis',1,'Z')
      call check_err(iret, 18)
      iret=nf_put_att_text(ncid,p_id,'positive',2,'up')
      call check_err(iret, 19)
      iret=nf_put_att_text(ncid,p_id,'long_name',6,'height')
      call check_err(iret, 20)
      iret=nf_put_att_text(ncid,p_id,'standard_name',6,'height')
      call check_err(iret, 21)      
      iret=nf_put_att_text(ncid,p_id,'units',2,'km')
      call check_err(iret, 22)
      iret=nf_put_att_text(ncid,time_id,'axis',1,'T')
      call check_err(iret, 23)      
      iret=nf_put_att_text(ncid,time_id,'long_name',4,'time')
      call check_err(iret, 24)
      iret=nf_put_att_text(ncid,time_id,'standard_name',4,'time')
      call check_err(iret, 25)
      iret=nf_put_att_text(ncid,time_id,'calendar',8,'standard')
      call check_err(iret, 26)
      iret=nf_put_att_text(ncid,time_id,'units',30,
     *     'days since 1950-01-01 00:00:00')
      call check_err(iret, 27)     
      
      ! leave define mode
      iret=nf_enddef(ncid)
      call check_err(iret, 32)

      ! store lon
      iret=nf_put_var_double(ncid,lon_id,lon)
      call check_err(iret, 33)

      ! store lat
	iret=nf_put_var_double(ncid,lat_id,lat)
      call check_err(iret, 34)

      ! store p
      iret=nf_put_var_double(ncid,p_id,p)
      call check_err(iret, 35)
       
      deallocate (lon,lat,p) 
      end


      subroutine netCDFWriteVariable(ncid,gas,ids,its,nh,rads,
     *      var_long_name,var_stand_name,var_units,file_name,time_id)

          ! Convert 3D data to proper netCDF format
          ! implicit none
          include 'netcdf.inc'

          ! error status return
          integer  iret,nh,its,ids

          ! netCDF id
          integer  ncid

          ! dimension ids
          integer  lon_dim,lat_dim,p_dim,time_dim,p_len

! dimension lengths
          integer,parameter :: n_time =1
          integer,parameter :: time_len=NF_UNLIMITED

! number of considered variables
	    character*80 var_name,var_units,exp_name 
	    character*60 var_stand_name,var_long_name
! file name
          character *100 file_name
          
          ! rank (number of dimensions) for each variable
          integer,parameter :: lon_rank =1
          integer,parameter :: lat_rank =1
          integer,parameter :: p_rank   =1
          integer,parameter :: time_rank=1
          integer,parameter :: var_rank =4    
          
          ! starts and counts for array sections of record variables
          integer  time_start(time_rank),time_count(time_rank)
          integer  var_start(var_rank),var_count(var_rank)
      
          ! variable ids
          integer  time_id,var_id
     
          ! variable shapes
          integer  var_dims(var_rank)
          integer  lon_dims  (lon_rank)
          integer  lat_dims  (lat_rank)
          integer  p_dims    (p_rank)
          integer  time_dims (time_rank)
          
          real*8  doubleval(1)

          ! data dimensions variables
          REAL(8), DIMENSION(:), allocatable :: lon  
          REAL(8), DIMENSION(:), allocatable :: lat
          REAL(8), DIMENSION(:), allocatable :: p
          ! data variables
          REAL(8), DIMENSION(:,:,:), allocatable :: var
      
          ! local variables
          integer i,j,ik,status,it,k,is,iy

          ! varibles 
	    real gas(ids,its,nh,n_time),rads(nh)
	    real*8 ytime(n_time),dtime(12,2),scale,start_ytime,day
	    real*8 xtime
          
          lon_len=ids
          lat_len=its
          p_len=nh
          
          allocate (lon(lon_len),lat(lat_len),p(p_len),var(ids,its,nh))

          ! откроем файл netCDF в который хотим дописать массив
          iret=nf_open(trim(file_name), NF_WRITE, ncid)
          call check_err(iret, 55)
          
          ! получим id необходимых размерностей
          iret=nf_inq_dimid(ncid, 'time', time_dim)
          call check_err(iret, 120)
          iret=nf_inq_dimid(ncid, 'plev', p_dim)
          call check_err(iret, 121)
          iret=nf_inq_dimid(ncid, 'lat', lat_dim)
          call check_err(iret, 122)
          iret=nf_inq_dimid(ncid, 'lon', lon_dim)
          call check_err(iret, 123)
          
          ! присвоим размерности массиву
          var_dims(4)=time_dim
          var_dims(3)=p_dim
          var_dims(2)=lat_dim
          var_dims(1)=lon_dim 

          ! объ€вл€ем новый массив
          iret=nf_redef(ncid)
          call check_err(iret, 51)
          iret=nf_def_var(ncid,var_stand_name,NF_DOUBLE,var_rank,
     *        var_dims,var_id)
	    call check_err(iret, 9)      
          iret=nf_put_att_text(ncid,var_id,'long_name',
     *        len(trim(var_long_name)),trim(var_long_name))
          call check_err(iret, 28)
          iret=nf_put_att_text(ncid,var_id,'standard_name',
     *        len(trim(var_stand_name)),trim(var_stand_name))
          call check_err(iret, 29)      
          iret=nf_put_att_text(ncid,var_id,'units',len(trim(var_units)),
     *        trim(var_units))
          call check_err(iret, 30)
          iret=nf_put_att_text(ncid,var_id,'cell_methods',30,
     *        'time:mean(of calendar months)')
          call check_err(iret, 31) 
          ! выходим из режима объ€влени€ нового массива
          iret=nf_enddef(ncid)
          call check_err(iret, 52)
          
          ! объ€вл€ем начальные значени€ дл€ нового массива
          var_start(1)=1
          var_start(2)=1
          var_start(3)=1
          var_count(1)=lon_len
          var_count(2)=lat_len
          var_count(3)=p_len
          
          ! записываем данные в новый массив netCDF файла
          do it=1,n_time

              irec=it
              xtime=ytime(it)

              var_start(4)=irec
              var_count(4)=1

              time_start(1)=irec
              time_count(1)=1

              iret=nf_put_vara_double(ncid,time_id,time_start,
     *            time_count,xtime)
              call check_err(iret, 39)
              var(1:lon_len,1:lat_len,1:p_len)=dble(gas(1:lon_len,
     *            1:lat_len,1:p_len,it))

	        iret=nf_put_vara_double(ncid,var_id,var_start,var_count,var)
              call check_err(iret, 37)
          enddo
          
          retval=nf_close(ncid)
          deallocate (var)
      
      end
      
      
      subroutine check_err(iret, number)
      
          integer iret
          include 'netcdf.inc'
          if (iret .ne. NF_NOERR) then
              write(*,*) nf_strerror(iret), number
              pause
          stop
          endif
      
      end

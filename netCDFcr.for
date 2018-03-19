! create netcdf for GSM TIP file
      subroutine netCDFcr(ncid,lon_len,lat_len,p_len,n_time,
     *     out_file_name,ddolgs,dtets)
      include 'netcdf.inc'
      integer p_len
* error status return
      integer  iret,iit,nh,its,ids

* netCDF id
      integer  ncid

* dimension ids
      integer  lon_dim
      integer  lat_dim
      integer  p_dim
      integer  time_dim

* dimension lengths


! number of considered variables

	character*10 var_name,var_units,exp_name 
	character*60 var_stand_name,var_long_name 

* variable ids
!
!      integer  lon_id
!      integer  lat_id
!      integer  p_id
!      integer  time_id
!      integer  var_id

* rank (number of dimensions) for each variable

      integer,parameter :: lon_rank =1
      integer,parameter :: lat_rank =1
      integer,parameter :: p_rank   =1
      integer,parameter :: time_rank=1

      integer,parameter :: time_len=NF_UNLIMITED
!      integer,parameter :: var_rank=4
     
* variable shapes

      integer  lon_dims  (lon_rank)
      integer  lat_dims  (lat_rank)
      integer  p_dims    (p_rank)
      integer  time_dims (time_rank)
!      integer  var_dims  (var_rank)

      real*8  doubleval(1)
      
* data variables
      real*8  lon(lon_len)
      real*8  lat(lat_len)
      real*8  p(p_len)

* starts and counts for array sections of record variables

      integer  time_start(time_rank),time_count(time_rank)
!      integer  var_start(var_rank),var_count(var_rank)

! local variables

       integer i,j,ik,status,it,k,is,iy
       
       character*100 out_file_name,in_file_name

       integer unlimdimid,irec

! varibles for reading oum-files
       

	real*8 ytime(n_time),dtime(12,2),scale,start_ytime,day
	real*8 xtime
!  day in month  
      data dtime(:,1) /31.,28.,31.,30.,31.,30.,31.,31.,30.,31.,30.,31./
      data dtime(:,2) /31.,29.,31.,30.,31.,30.,31.,31.,30.,31.,30.,31./
          
  

      do j=1,n_time
      ytime(j)=j*1.0  
      enddo
! ----------- Grid definition
       do i= 1,lon_len

       lon(i)=(i-1)*ddolgs

       enddo

       do i= 1,lat_len

       lat(i)=90. - (i-1)*dtets

       enddo

! ----------- Variable definition      
	 	 

       var_stand_name='air_temperature'       
       var_units='K'       
       
	 
! read gases from the the input file  ???
! here I put artificial data

  
	  !write(*,*) 'read is ok'
	  
* attribute vectors

* enter define mode

      iret=nf_create(trim(out_file_name),NF_CLOBBER,ncid)
      
      call check_err(iret, 0)

      !write(*,*) 'nf_create is ok'
      return 
      end
      
* define dimensions

 !     iret=nf_def_dim(ncid,'lon',lon_len,lon_dim)
 !     call check_err(iret)

!	iret=nf_def_dim(ncid,'lat',lat_len,lat_dim)
 !     call check_err(iret)
!
!	iret=nf_def_dim(ncid,'plev',p_len,p_dim)
!      call check_err(iret)!

	!iret=nf_def_dim(ncid,'time',NF_UNLIMITED,time_dim)
      !call check_err(iret)

      !  write(*,*) 'point 1'
      !return 
      !end

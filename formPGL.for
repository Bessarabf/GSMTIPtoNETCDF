      subroutine formPGL(oinf,pole,ldor,numr,fnuname)
      PARAMETER num_dim=11
      dimension pole(ldor),kdf(20),kdu(20),
     *          oinf(ldor),rads(30)
      integer:: numpar(11)
      integer time_id
! массивы для распределения шара pars, pot, gins
      allocatable pars(:,:,:,:), !  новый шар
     *            pot(:,:,:),  ! potensial
     *            gins(:,:,:,:)
! 3d massive for nc-file writing   
      character*80, chlongName(20),chName(20),chUnit(20),fnuname
      character*100 file_name
      allocatable gas(:,:,:,:)
      data numpar/1,2,3,4,6,7,10,11,12,13,14/
      data  am1,am2,am3/53.12e-24,46.51e-24,26.56e-24/,
     *      BK/1.38e-16/
     
      file_name=trim(fnuname)//'.nc'

      chName(1)='O2'
      chUnit(1)='cm-3'
      chlongName(1)='O2 density'
      chName(2)='N2'
      chUnit(2)='cm-3'
      chlongName(2)='N2 density'
      chName(3)='O'
      chUnit(3)='cm-3'
      chlongName(3)='O density'
      chName(4)='NO'
      chUnit(4)='cm-3'
      chlongName(4)='NO density'
      chName(5)='Tn'
      chUnit(5)='K'
      chlongName(5)='temperature'
      chName(6)='mol+'
      chUnit(6)='cm-3'
      chlongName(6)='mol+'
      chName(7)='vr'
      chUnit(7)='cm/c'
      chlongName(7)='vert wind'
      chName(8)='vtet'
      chUnit(8)='cm/c'
      chlongName(8)='meridional wind'
      chName(9)='vfi'
      chUnit(9)='cm/c'
      chlongName(9)='zonal wind'
      chName(10)='ro'
      chUnit(10)='g/cm-3'
      chlongName(10)='neutral density'
      chName(11)='P'
      chUnit(11)='Pa'
      chlongName(11)='Pressure'
      chName(12)='O+'
      chUnit(12)='1/cm-3'
      chlongName(12)='O+ density'
      chName(13)='Vz(O+)'
      chUnit(13)='cm/s'
      chlongName(13)='vert wind O+'
      chName(14)='Vtet(O+)'
      chUnit(14)='cm/s'
      chlongName(14)='meridional wind O+'
      chName(15)='Vfi(O+)'
      chUnit(15)='cm/s'
      chlongName(15)='zonal wind O+'
      chName(16)='Te'
      chUnit(16)='K'
      chlongName(16)='electron temperature'
      chName(17)='Ti'
      chUnit(17)='K'
      chlongName(17)='ion temperature'
      
! формируем kdf И kdu из информ записи
      kdf(1:20)=pole(10:29) 
	kdu(1:20)=pole(30:49)
      print*,'kdu=',kdu
      rads=pole(106:135)
! формируем размерности массивов
    	! 5х5 сетки ... 
 ! shar parameters     ! 
      kpars=oinf(51) 
      nh=oinf(60)
      dtets=oinf(57)
      DDOLGs=oinf(56)
      its=(180./dtets+0.1)+1
      ids=360./DDOLGs+0.01
      ksh=kpars*nh*its*ids
! interpolation parameters       
      ins=oinf(53)
      int=oinf(52)
      kpart=oinf(50)
      DDOLGt=oinf(54)
      idt=360./DDOLGt+0.01
      ntr=pole(58) 
      nl=pole(61) 
      ntpl=0
      
! potential
      kpot=ntr*its*ids 
      kinsh=ins*nh*its*ids
   
      allocate(pars(kpars,nh,its,ids),pot(ntr,its,ids),
     *         gins(ins,nh,its,ids))
      allocate( gas(ids+1,its,nh,1))
!!!!!!!!!!!!!! формируем потенциал	
!      call formPot(pot,
!     *             oinf,pole,kdf,kdu,
!     *             ntr,idt,ids,ldor) 
 
	! print *, 'inf potensial' 
!!!!!! формируем шар !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      call  globrw(pars,kpars,ldor*4,
     *               nh,kdf,ksh,its,ids,numr)

	! print *,pars(7,1:30,2,2)
      call filegeo(pars,nh,its,ids,kpars,dtets,ddolgs,
     *          rads)
      call bongi(pars,kpars,nh,its,ids)
      it=1
      n_time=1
      call netCDFcr(ncid,ids+1,its,nh,n_time,file_name,ddolgs,dtets)
      call netCDFWriteGrid(ncid,gas,ids+1,its,nh,it,rads,file_name,
     *                     time_id)
      retval=nf_close(ncid)
      print*,ncid
      do index=1,11 !kpars
         kp=numpar(index)
          do j=1,ids+1
             jp=j
             if(j.eq.ids+1) jp=1
            do i=1,its
               
               if(index.eq.10) then
                  gas(j,i,1:nh,1)=am1*pars(1,1:nh,i,jp)+
     *                            am2*pars(2,1:nh,i,jp)+ 
     *                            am3*pars(3,1:nh,i,jp)
               elseif (index.eq.11) then
                  gas(j,i,1:nh,1)=(pars(1,1:nh,i,jp)+
     *                            pars(2,1:nh,i,jp)+ 
     *                            pars(3,1:nh,i,jp))*
     *                       pars(7,1:nh,i,jp)*BK*1.e-1 
               else
                   gas(j,i,1:nh,1)=pars(kp,1:nh,i,jp)
               end if
            end do
          end do
         call netCDFWriteVariable(ncid,gas,ids+1,its,nh,rads,
     *        chlongName(index),chName(index),chUnit(index),
     *        file_name,time_id)

      end do  
! gins
      call formgins(gins,rads,ins,nh,its,ids,numr,kdf,kdu )
! Ti & Te shar 
      gins(5,1:15,1:its,1:ids)=pars(9,1:15,1:its,1:ids)
      gins(6,1:15,1:its,1:ids)=pars(8,1:15,1:its,1:ids)
      call filegeo(gins,nh,its,ids,ins,dtets,ddolgs,
     *          rads)
c . . . data in poles
       call bongi(gins,ins,nh,its,ids)


      do index=12,17 !
         kp=index-11 ! interpolation parameters of "SHAR" 
         print *,'ins=',kp
         do j=1,ids+1
            jp=j
            if(j.eq.ids+1) jp=1
            do i=1,its
              gas(j,i,1:nh,1)=gins(kp,1:nh,i,jp)
              
            end do
         end do
      
         call netCDFWriteVariable(ncid,gas,ids+1,its,nh,rads,
     *        chlongName(index),chName(index),chUnit(index),
     *        file_name,time_id)
      end do  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


	deallocate (pars,pot,gins,gas)
	end

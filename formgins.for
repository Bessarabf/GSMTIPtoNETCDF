	subroutine formGINS(gins,rads,ins,nh,its,ids,numr,kdf,kdu)
	
	dimension gins(ins,nh,its,ids),kdf(20),kdu(20),rads(nh)
	
	
	npgl=ins*nh*its*ids

	ldor=4096
	!
	dtets=180.0/(its-1)
	ddolgs=360.0/ids
      call  globGINS(gins,rads,ins,ldor,
     *                nh,kdf,kdu,npgl,its,ids,numr)
      open(99,file='surf3.dat')
      call write_surf(gins,nh,its,ids,ins,dtets,ddolgs)
      close(99)

!      call filegeo(gins,nh,its,ids,ins,dtets,ddolgs,
!     *          rads)
	call bongi(gins,ins,nh,its,ids)
	! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       
	 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   	return

	end

	 
!���������� ������ 2004
! nfile - ����������  ����� ����� - 4 ��� 5, ������ � open
! readfl - ���������� ������� ������ � ���� (false)��� ������ (truth)
! pgl1 - ������ ���!��������� kpar*nh*its*ids
! nh - ����� ����� �� ������
! ids - ����� ����� �� �������
! its -����� ����� �� ������
! mass- ������ ����������� ����������
! ldor - ����� ������ - 4096 (4*1024) �����
! kdf - ������ � �������� ��������� �������. �������� �������� ��������� ����, ������ � �.�.
!  
      subroutine globGINS(pgl1,rads,kpar,ldor,
     *                  nh,kdf,kdu,npgl,its,ids,numr)
      !include 'parametr.inc'
      dimension pgl1(npgl),kdf(20),kdu(20),pole(ldor/4),rads(NH)
      
!
 
      print *,' globGINS - begin'
      npg=kpar*nh*its*ids
	print*,npg,npgl,ldor/4
!  �������� ������������ ����� ������� � ���������� ����������. 
! ���������� ��� ������������ ������ File4
      if (npgl.lt.npg) then 
        print 700,npg,npgl
700     format (' gloGINS :   ********  error  ********'/
     *   '  npg=',i8,'  >   npgl=',i8,'  !!!!!!  STOP  !')
        stop
      end if  
      
! �⥭�� 1-� ����� � ���ᨢ� ����
      read(numr,rec=1) pole
	rads=pole(106:135)

! ����� ��砫쭮� ����� ���������������� ��ࠬ��஢ �� KDU(7)

      isp=kdf(7)+1
      mdor=ldor/4    
         k=1
         do j=1,kdu(7)-1
           read(numr,rec=isp) pole
	  
           do i=1,mdor
             pgl1(k)=pole(i)
             k=k+1
           end do 
           isp=isp+1
         end do 
      ! ��᫥���� ������
	
	  read(numr,rec=isp) pole
	  pgl1(k:npg)=pole(1:mdor)

      print *, 'globGINS - end'
      return
      end


	 
	  subroutine write_surf(pglS,nh,its,ids,ins,dtets,ddolgs)
	  
	  dimension pglS(ins,nh,its,ids),rads(nh)
	  tet=90.
	  do i=1,its
	    dolg=0
		do j=1,ids
		  write(99,'(2F8.2,6(2X,1PE9.2))') dolg,tet,(pglS(kp,30,i,j),kp=1,ins)
		  dolg=dolg+ddolgs
		end do
		tet=tet-dtets
	   end do 
	   return
	   end

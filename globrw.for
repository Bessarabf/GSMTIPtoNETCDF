! ���������� ������ 2004
! nfile - ����������  ����� ����� - 4 ��� 5, ������ � open
! readfl - ���������� ������� ������ � ���� (false)��� ������ (truth)
! pgl1 - ������ ������������ kpar*nh*its*ids
! nh - ����� ����� �� ������
! ids - ����� ����� �� �������
! its -����� ����� �� ������
! mass- ������ ����������� ����������
! ldor - ����� ������ - 4096 (4*1024) �����
! kdf - ������ � �������� ��������� �������. �������� �������� ��������� ����, ������ � �.�.
!  
      subroutine globrw(pgl1,kpar,ldor,
     *                  nh,kdf,npgl,its,ids,numr)
      !include 'parametr.inc'
      dimension pgl1(npgl),kdf(20),pole(ldor/4),rads(NH)
      
!
 
      ! print *,' globrw - begin, ldor=',ldor
      ! pause
      npg=kpar*nh*its*ids
      ! print*,npg,kpar,nh,its,ids
! �������� ������������ ����� ������� � ���������� ����������. 
! ���������� ��� ������������ ������ File4
      if (npgl.lt.npg) then 
        ! print 700,npg,npgl
700     format (' globrw :   ********  error  ********'/
     *   '  npg=',i8,'  >   npgl=',i8,'  !!!!!!  STOP  !')
        stop
      end if  
      


      isp=kdf(5)+1
      ! print*,'isp=',isp
      ! pause
      mdor=ldor/4     ! ����� ������������ �������� � ������ (4 ����� �� �����)
      ndor=npgl/mdor  ! ����� ������� (ndor - ����� ��������� � 4� ������ ������ 
      nost=npgl-ndor*mdor      ! �������
                      ! ������  
      k=1
         do j=1,ndor
           ! print *,j,isp
           read(numr,rec=isp) pole
           do i=1,mdor
             pgl1(k)=pole(i)
             k=k+1
           end do 
           isp=isp+1
         end do 
         if(nost.NE.0) then
           read(numr,rec=isp)pole
           do i=1,nost            ! ������ �������
             pgl1(k)=pole(i)
             k=k+1
           end do 
         end if
      
      ! print *, 'globrw - end'
      return
      end

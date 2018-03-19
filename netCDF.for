      program file4tonetCDF
! form netCDF file from file4 (5x5 grid)
      include 'kdf_kdu.inc'
      PARAMETER(ldor0=1024)
      dimension oinf5(ldor0),pole(ldor0)
      character namef*80, fnuName*80
      numr=4

      print*,' form netCDF file from file4'
      print*,' input F4 file''s name 5�5'
      read(*,*) fnuName
      mdor=ldor0*4
! ����� ��� �������
! ���������, ���� �� ������ ����� f4 
      open(numR,file=trim(fnuName),status='old',access='direct',
     *     form='unformatted',recl=mdor)
! ������ ���� f4
! ��������� �� �������������� ������ ��������� �����. 
! ����� ���������� ���� ������ ����=19
! ����� ���������������� ���������� ������ ������ ����=14 

	call analsInf(numr,oinf5,pole,ldor0,kode)
	if (kode.eq.1) then
	   print*,'Incorrect kpars or ints in '//nameF//' and '//fnuName
	   stop
	end if
 	
! - ��������� ������ ���������� � 4-� ������ ������� ���� � ��������.����
! ������ shar, trubk � pot
         call formPGL(oinf5,pole,ldor0,numr)
      	 
	  ! ����� 
        close(numR) 
	  end



!!!!
      subroutine analsInf(numr,oinf5,pole,ldor,kode)
      ! ������ �������������� ������
	dimension oinf5(LDOR),pole(ldor) 
      read(numr,rec=1) pole
	! ���������� ���������� ���� � ���������������� ���������� ������
	kpars =pole(51)
      int=pole(52)
	  if((kpars.ne.19).or.(int.ne.14)) then 
	     print *,'Kpars= ',kpars, ' int=',int
	     kode=1
	     return
	  end if
 	  oinf5(1:LDOR)=POLE(1:LDOR)
      return
      end

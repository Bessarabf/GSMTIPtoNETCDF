       subroutine formShar(pars,oinf,pole,
     *                    kdf,kdu,kpars,
     *                    nh,its,ids,ldor,nadrs)

	dimension  pars(kpars*nh*its*ids),
     *          kdf(20),kdu(20),
     *          oinf(ldor),pole(ldor)
	! ������ � ��������� ������ ����
 	kSh5=kpars*nh*its*ids
	print*,kpars,nh,its,ids,ldor,nadrs
      pause
	j16=1 	! ������ ������ ����
	do i=1,kdu(5)-1
		lrec=kdf(5)+i
		read(4,rec=lrec) pole
		pars(j16:j16+ldor-1)=pole(1:ldor)
	    j16=j16+ldor
          print *,'lrec=',lrec
	end do

	! ������ ��������� ������ ���� ��������� ������ ������� ���� � ����� 
      ! ������������ �������
	ndr=ksh5/ldor
      nzaps=ndr+1
      no=ksh5-ndr*ldor ! �������
      nadrs=no+1
      lrec=lrec+1
	
	read(4,rec=lrec) pole
      print *,lrec
      pars(j16:16+nadrs-1)=pole(1:nadrs-1)
      print *,(pole(1:nadrs-1))
    	!POLE(nadrs:ldor)=0.

      !              ������ ���� � �������
      ut0=pole(nadrs)
      ut1=pole(nadrs+1)
      day=pole(nadrs+2)
      god=pole(nadrs+3) 

	print*,' ut0  ',' ut1  ',' day ',' god ' 
      print*,	 ut0,ut1,day,god

	! ����������� ������ ���� 
	
	pars(j16:j16+no-1)=pole(1:no) ! ������ "���" �����
	
	! ������������� � ����� ���
 	return
	end

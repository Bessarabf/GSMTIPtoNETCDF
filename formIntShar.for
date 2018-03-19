      subroutine formIntShar(gins_5,oinf5,pole,
     *                    kdf5,kdu5,ins_5,
     *                    nh_5,its_5,ids_5,ldor)
 
       dimension  gins_5(ins_5*nh_5*its_5*ids_5),
     *          kdf5(20),kdu5(20),
     *          oinf5(ldor),pole(ldor)
	!читаем и формируем массив "старого" шара
 	kins5=ins_5*nh_5*its_5*ids_5

	j16=1
	do i=1,kdu5(7)-1
		lrec=kdf5(7)+i
		read(4,rec=lrec) pole
		gins_5(j16:j16+ldor-1)=pole(1:ldor)
	    j16=j16+ldor
	end do

	! читаем последнюю запись 
      lrec=lrec+1
	read(4,rec=lrec) pole
	! рассчитываем остаток
	ndr=kins5/ldor
      no=kins5-ndr*ldor
	! присваиваем данные  
	gins_5(j16:j16+no-1)=pole(1:no) ! старый gins готов
	return
	end


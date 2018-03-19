      program file4tonetCDF
! form netCDF file from file4 (5x5 grid)
      include 'kdf_kdu.inc'
      PARAMETER(ldor0=1024)
      dimension oinf5(ldor0),pole(ldor0)
      character namef*80, fnuName*80
      numr=4

      print*,' form netCDF file from file4'
      print*,' input F4 file''s name 5х5'
      read(*,*) fnuName
      mdor=ldor0*4
! файлы уже созданы
! провер€ем, есть ли нужные файлы f4 
      open(numR,file=trim(fnuName),status='old',access='direct',
     *     form='unformatted',recl=mdor)
! „итаем файл f4
! провер€ем по информационной записи параметры файла. 
! число параметров шара должны быть=19
! число интерпол€ционных параметров трубки должно быть=14 

	call analsInf(numr,oinf5,pole,ldor0,kode)
	if (kode.eq.1) then
	   print*,'Incorrect kpars or ints in '//nameF//' and '//fnuName
	   stop
	end if
 	
! - формируем массив потенциала и 4-х мерные массивы шара и интерпол.шара
! „итаем shar, trubk и pot
         call formPGL(oinf5,pole,ldor0,numr)
      	 
	  ! конец 
        close(numR) 
	  end



!!!!
      subroutine analsInf(numr,oinf5,pole,ldor,kode)
      ! читаем информационную запись
	dimension oinf5(LDOR),pole(ldor) 
      read(numr,rec=1) pole
	! количество параметров шара и интерпол€ционных параметров трубки
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

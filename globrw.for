! исправлено ноябрь 2004
! nfile - логический  номер файла - 4 или 5, описан в open
! readfl - логическое условие запись в файл (false)или чтение (truth)
! pgl1 - массив размерностью kpar*nh*its*ids
! nh - число узлов по высоте
! ids - число узлов по долготе
! its -число узлов по широте
! mass- массив управляющих параметров
! ldor - длина записи - 4096 (4*1024) байта
! kdf - массив с номерами начальных записей. Помогает выделить параметры шара, трубки и т.д.
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
! проверка соответствия длины массива и количества параметров. 
! Необходимо для соответствия версий File4
      if (npgl.lt.npg) then 
        ! print 700,npg,npgl
700     format (' globrw :   ********  error  ********'/
     *   '  npg=',i8,'  >   npgl=',i8,'  !!!!!!  STOP  !')
        stop
      end if  
      


      isp=kdf(5)+1
      ! print*,'isp=',isp
      ! pause
      mdor=ldor/4     ! число вещественных значений в записи (4 байта на число)
      ndor=npgl/mdor  ! число записей (ndor - число элементов в 4х мерном массив 
      nost=npgl-ndor*mdor      ! остаток
                      ! чтение  
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
           do i=1,nost            ! читаем остаток
             pgl1(k)=pole(i)
             k=k+1
           end do 
         end if
      
      ! print *, 'globrw - end'
      return
      end

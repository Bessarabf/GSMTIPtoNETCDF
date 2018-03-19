c
c   INCLUDE-файл, задающий размерности массивов глобальной модели.
c   Для перехода на новую сетку нужно изменить константы только здесь.
c   Здесь же задается длина записи на диске.
c
      PARAMETER (LDOR0=4096,
     *     KPART0=8, KPARS0=19, INT0=12, INS0=6,
     *     NSU0=24,  NSE0=15,   NTR0=16, IDDOLGS=5, IDDOLGT=5,
     *     NV0=140,  IDS0=360/IDDOLGS,   IDT0=360/IDDOLGT, NH0=30,

     *     IDTETS=5, IDTETT=5,
     *     ITS0=(180/IDTETS+1), NL0=90/IDTETT-1, NL20=NL0*2+3,
     *     NTPL0=1432, NI0=NTPL0*INT0, KS0=NTPL0*2,
     *     NR0=NH0*ITS0*KPARS0)

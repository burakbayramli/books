
      real*8           ab     ,abm     ,dab
      common /isog_sh/ ab(3,3),abm(2,2),dab(3,2,2)

      real*8           as     ,asm     ,das
      common /isog_sh/ as(3,3),asm(2,2),das(3,2,2)

      real*8           us     ,usm     ,dus
      common /isog_sh/ us(3,3),usm(2,2),dus(3,2,2)

      real*8           nb   , ns   , eb     , es
      common /isog_sh/ nb(3), ns(3), eb(3,3), es(3,3)

      real*8           qs1     , a11q1   ,a22q1   ,a12q1   , dns
      common /isog_sh/ qs1(3,3), a11q1(3),a22q1(3),a12q1(3), dns(3,2)

      real*8           qs2     , a11q2   ,a22q2   ,a12q2   , dnb
      common /isog_sh/ qs2(3,3), a11q2(3),a22q2(3),a12q2(3), dnb(3,2)

      real*8           sab       , cab1       ,cab2       , d1   ,d2
      common /isog_sh/ sab(3,2,2), cab1(3,2,2),cab2(3,2,2), d1(3),d2(3)

      real*8           aa11     , aa12     , aa21     , aa22
      common /isog_sh/ aa11(3,3), aa12(3,3), aa21(3,3), aa22(3,3)

      real*8           nfor      , xlq
      common /isog_sh/ nfor(6,64), xlq(3,64)

      real*8           bab
      common /isog_sh/ bab(3)

      real*8           eps   , chi   , f       , detf
      common /isog_sh/ eps(3), chi(3), f(3,3,4), detf(4)

      real*8           neps
      common /isog_sh/ neps(6,64)

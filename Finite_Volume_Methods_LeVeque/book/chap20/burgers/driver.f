      program driver
c
c  Generic driver routine for claw2
c
c  Author: Randall J. LeVeque
c  Version of March, 1999 --  CLAWPACK Version 4.0
c
c
      implicit double precision (a-h,o-z)

c     # set parameters for maximum array sizes used in declarations
c     # these must be increased for larger problems.
c
c
      parameter (maxmx =   300)
      parameter (maxmy =   300)
      parameter (mwork =   96672)


      parameter (mbc = 2)
      parameter (meqn = 1)
      parameter (mwaves = 1)
      parameter (maux = 0)   
c       # NOTE: if maux>0 you must declare aux properly below!

      dimension    q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)

      dimension  aux(1)   !# dummy variable since no aux arrays used
c     dimension  aux(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, maux)

      dimension mthlim(mwaves)
      dimension work(mwork)
c
c
      call claw2ez(maxmx,maxmy,meqn,mwaves,mbc,maux,mwork,mthlim,
     &           q,work,aux)

      stop 
      end

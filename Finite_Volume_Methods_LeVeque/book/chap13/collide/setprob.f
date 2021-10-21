      subroutine setprob
      implicit double precision (a-h,o-z)
      common /cqinit/ sloc,hl,ul,hr,ur
      common /comrp/ grav
c
c     # Shallow water equations
c
      open(unit=7,file='setprob.data',status='old',form='formatted')

c     # Graviational constant g:
      read(7,*) grav
c
c
      return
      end

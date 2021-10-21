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

c     # initial data is a single discontinuity at sloc
      read(7,*) sloc
      read(7,*) hl
      read(7,*) ul
      read(7,*) hr
      read(7,*) ur
c
      return
      end

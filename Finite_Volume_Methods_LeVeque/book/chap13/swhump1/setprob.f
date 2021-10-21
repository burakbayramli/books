      subroutine setprob
      implicit double precision (a-h,o-z)
      common /cqinit/ beta,ic
      common /comrp/ grav
c
c     # Shallow water equations
c
      open(unit=7,file='setprob.data',status='old',form='formatted')

c     # Graviational constant g:
      read(7,*) grav
c

c     # choice of initial data:
      read(7,*) ic
c     # beta for initial conditions:
      read(7,*) beta
c
      return
      end

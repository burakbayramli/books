      subroutine setprob
      implicit double precision (a-h,o-z)
      common /cqinit/ beta,ic
      common /comaux/ rhol,cl,rhor,cr
      common /comlim/ mylim,mrplim(2)
c
c     # Set the material parameters for the acoustic equations
c
      open(unit=7,file='setprob.data',status='old',form='formatted')

c     # choice of initial data:
      read(7,*) ic
c     # beta for initial conditions:
      read(7,*) beta
c
c     # Piecewise constant medium with single interface at x=0
c     # Density and sound speed to left and right:
      read(7,*) rhol
      read(7,*) cl
      read(7,*) rhor
      read(7,*) cr

      return
      end

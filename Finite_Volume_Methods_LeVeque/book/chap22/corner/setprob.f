      subroutine setprob
      implicit double precision (a-h,o-z)
      common /comaux/ rho1,amu1,alam1,rho2,amu2,alam2

c
c     # Set the material parameters for the elasticity equations
c
      open(unit=7,file='setprob.data',status='old',form='formatted')

c
c     # Piecewise constant medium 
c     # Material parameters

      read(7,*) rho1
      read(7,*) alam1
      read(7,*) amu1

      read(7,*) rho2
      read(7,*) alam2
      read(7,*) amu2

      return
      end

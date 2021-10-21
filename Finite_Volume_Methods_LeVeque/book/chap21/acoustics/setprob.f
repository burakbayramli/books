      subroutine setprob
      implicit double precision (a-h,o-z)
      common /cparam/ rho,bulk,cc,zz

c
c     # Set the material parameters for the acoustic equations
c
      open(unit=7,file='setprob.data',status='old',form='formatted')
c
c     # Density and sound speed:

      read(7,*) rho
      read(7,*) cc
c
c     # Compute bulk modulus and impendance:

      bulk = cc*cc*rho
      zz = rho*cc

      return
      end

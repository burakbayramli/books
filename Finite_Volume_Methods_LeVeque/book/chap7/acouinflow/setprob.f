      subroutine setprob
      implicit double precision (a-h,o-z)
      common /cparam/ rho,bulk,cc,zz   
      common /combc/ omega
c
c     # Set the material parameters for the acoustic equations
c     # Passed to the Riemann solver rp1.f in a common block
c
      open(unit=7,file='setprob.data',status='old',form='formatted')


c     # density:
      read(7,*) rho

c     # bulk modulus:
      read(7,*) bulk

c     # sound speed:
      cc = dsqrt(bulk/rho)

c     # impedance:
      zz = cc*rho

c     # frequency omega for boundary conditions:
      read(7,*) omega

      return
      end

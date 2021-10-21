      subroutine setprob
      implicit double precision (a-h,o-z)
      common /comrp/ u
      common /comic/ beta
      common /comsrc/ rate
c
c     # Set the velocity for scalar advection
c     # This value is passed to the Riemann solver rp1.f in a common block
c
c     # Set the width of the initial Gaussian pulse
c     # beta is passed to qinit.f in comic
c
c     # Set the rate in the source term
c     # beta is passed to src1.f in comsrc
c
      open(unit=7,file='setprob.data',status='old',form='formatted')

      read(7,*) u
      read(7,*) beta
      read(7,*) rate

      return
      end

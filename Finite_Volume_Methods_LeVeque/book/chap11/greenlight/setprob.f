      subroutine setprob
      implicit double precision (a-h,o-z)
      common /comrp/ umax
      common /comic/ beta
c
c     # Set the maximum velocity for traffic flow problem
c     # This value is passed to the Riemann solver rp1.f in a common block
c
c     # Set the width of the initial Gaussian pulse
c     # beta is passed to qinit.f in comic
c
      open(unit=7,file='setprob.data',status='old',form='formatted')

      read(7,*) umax
      read(7,*) beta

      return
      end

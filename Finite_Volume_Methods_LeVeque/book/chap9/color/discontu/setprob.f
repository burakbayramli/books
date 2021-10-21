      subroutine setprob
      implicit double precision (a-h,o-z)
      common /comic/ beta, freq
c
c     # Set the velocity for scalar advection
c     # This value is passed to the Riemann solver rp1.f in a common block
c
c     # Set the width of the Gaussian for the wave packet
c     # beta is passed to qinit.f in comic
c
c     # Set the frequency of the wave packet
c     # freq is passed to qinit.f in comic
c
      open(unit=7,file='setprob.data',status='old',form='formatted')

      read(7,*) beta
      read(7,*) freq

      return
      end

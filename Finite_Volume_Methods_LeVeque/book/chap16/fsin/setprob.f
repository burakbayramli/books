      subroutine setprob
      implicit double precision (a-h,o-z)
      common /comic/ ur, ul
c
c     # Set the Riemann initial values
c     # This value is passed to qinit1.f in the common block comic
c
      open(unit=7,file='setprob.data',status='old',form='formatted')

      read(7,*) ur
      read(7,*) ul

      return
      end

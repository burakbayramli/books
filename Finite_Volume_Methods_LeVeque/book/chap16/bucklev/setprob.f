      subroutine setprob
      implicit double precision (a-h,o-z)
      common /comprob/ a
      common /comic/ ur, ul
c
c     # Set the Riemann initial values vor Buckley-Leverett equation
c     # This value is passed to qinit1.f in the common block comic
c
c     # Set the parameter value a used in the flux function
c     # a is passed to rp1.f in comprob
c
      open(unit=7,file='setprob.data',status='old',form='formatted')

      read(7,*) ur
      read(7,*) ul
      read(7,*) a

      return
      end

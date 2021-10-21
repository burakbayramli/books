c
c
c
c     =================================================
      function fdisc(x,y)
c     =================================================

      implicit double precision (a-h,o-z)

c
c     # for computing cell averages for initial data that has a
c     # discontinuity along some curve.  fdisc should be negative to the 
c     # left of the curve and positive to the right

c     # half wedge
      if (x .gt. 0.d0 .and. (y.lt.(0.55d0*x))) then
          fdisc = 1.d0
	else
	  fdisc = -1.d0
	endif
c
      return
      end

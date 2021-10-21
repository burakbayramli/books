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

      fdisc = 1.d0
      if (x .gt.0.5d0 .and. x.lt.1.5d0 .and. y.gt.0.4d0 
     &   .and. y.lt.0.6d0) fdisc = -1.d0
c
      return
      end

!$Id:$
      integer function findsegm(u, knot, lknot )

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute segment number accounting for repeated knots

!      Inputs:
!        u       - Knot location
!        knot(*) - Knot vector
!        lknot   - Knot vector length

!      Output:
!        findseg  - Interval of knot
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'iofile.h'

      integer (kind=4) ::lknot,l
      real    (kind=8) ::knot(*), u

      findsegm = 0
      do l = 2,lknot
        if(knot(l).gt.knot(l-1)) then
          if(u.ge.knot(l-1)) then
            findsegm = findsegm + 1
          else
            return
          endif
        endif
      end do ! l

      end

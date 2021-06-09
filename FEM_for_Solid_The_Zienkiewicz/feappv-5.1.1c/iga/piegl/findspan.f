!$Id:$
      function findspan(n,u,p, bu)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: Determine the knot span index

!      Algorithm A2.1: The NURBS Book, Page 68

!      Inputs : n, u,p, bu(0:*)
!        n     = No. CP -1
!        u     = pt
!        p     = order
!        bu(*) = knot vector

!      Outputs: knot span index in 'findspan'
!        findspan
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      integer   findspan, n,p, low, high, mid
      real*8    u, bu(0:*)

!     Do binary search

      if(u .ge. bu(n)) then
        findspan = n - p
      else
        low  = p
        high = n + 1
        mid = (low + high)/2
        do while (u .lt. bu(mid) .or. u .ge. bu(mid+1))
          if(u .lt. bu(mid)) then
            high = mid
          else
            low  = mid
          endif
          mid = (low + high)/2
        end do ! while
        findspan = mid
      endif

      end

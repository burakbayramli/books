!$Id:$
      function fspan(n,u,p,u_knot)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Coded by:    Robert L. Taylor
!      Date:        January 11, 2006
!      Release:     1.0

!      Modified by: Rossana Dimitri
!      Date:        February 08, 2012
!      Release:     1.0

!      Purpose: Determine the u_knot span index

!      Algorithm A2.1: The NURBS Book, Page 68

!      Inputs : n, u, p, u_knot(n+p+1)

!      Outputs: u_knot span index in 'fspan'
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      integer   fspan, n, p, low, high, imid
      real*8    rmid, u, u_knot(n+p+1)

!     Do binary search

      if(u .eq. u_knot(n+p+1)) then
         fspan = n
      else
         low  = p
         high = n + 1
         rmid = (low + high)/2
         imid = nint(rmid)
         do while (u < u_knot(imid) .or. u >= u_knot(imid+1))
            if(u < u_knot(imid)) then
               high = imid
            else
               low  = imid
            endif
            imid = (low + high)/2
         end do ! while
         fspan = imid
      endif

      end

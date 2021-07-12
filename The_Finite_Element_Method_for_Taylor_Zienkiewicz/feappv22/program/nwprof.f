c$Id:$
      subroutine nwprof(jp,neq)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Sum column heights to form profile pointer jp

c      Inputs:
c         jp(*)  - Column heights
c         neq    - Number of equations

c      Outputs:
c         jp(*)  - Profile pointer array
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer   neq, n
      integer   jp(*)

      jp(1) = 0
      do n = 2,neq
        jp(n) = jp(n) + jp(n-1)
      end do

      end

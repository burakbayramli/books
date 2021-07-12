c$Id:$
      subroutine colred(au,xj,nn, b)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Columnwise reduction for back substitution

c      Inputs:
c         au(*)   - Upper column of reduced array A
c         xj      - Solution of reduced column
c         nn      - Length to reduce
c         b(*)    - Unreduced column

c      Outputs:
c         b(*)    - Reduced column
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer   n,nn
      real*8    xj, au(*),b(*)

      do n = 1,nn
        b(n) = b(n) - au(n)*xj
      end do

      end

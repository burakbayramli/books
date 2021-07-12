c$Id:$
      subroutine colbac(u,s,d,jj)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Backsubstitution macro for eigen solution

c      Inputs:
c         s(*)  - Unreduced column
c         u(*)  - Column of upper array already reduced
c         d     - Solution value for 'u' column
c         jj    - Length to reduce

c      Outputs:
c         s(*)  - Reduced column
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer   j,jj
      real*8    d,u(*),s(*)

      do j = 1,jj
        s(j) = s(j) - u(j)*s(jj+1)
      end do
      s(jj) = s(jj)*d

      end

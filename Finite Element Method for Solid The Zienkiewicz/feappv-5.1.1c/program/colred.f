!$Id:$
      subroutine colred(au,xj,nn, b)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Columnwise reduction for back substitution

!      Inputs:
!         au(*)   - Upper column of reduced array A
!         xj      - Solution of reduced column
!         nn      - Length to reduce
!         b(*)    - Unreduced column

!      Outputs:
!         b(*)    - Reduced column
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: n,nn
      real (kind=8) :: xj, au(*),b(*)

      do n = 1,nn
        b(n) = b(n) - au(n)*xj
      end do

      end subroutine colred

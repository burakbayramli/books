!$Id:$
      subroutine colbac(u,s,d,jj)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Backsubstitution macro for eigen solution

!      Inputs:
!         s(*)  - Unreduced column
!         u(*)  - Column of upper array already reduced
!         d     - Solution value for 'u' column
!         jj    - Length to reduce

!      Outputs:
!         s(*)  - Reduced column
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: j,jj
      real (kind=8) :: d,u(*),s(*)

      do j = 1,jj
        s(j) = s(j) - u(j)*s(jj+1)
      end do
      s(jj) = s(jj)*d

      end subroutine colbac

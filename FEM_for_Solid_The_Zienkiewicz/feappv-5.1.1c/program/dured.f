!$Id:$
      function dured(al,au,ad,jh)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Reduce diagonal in unsymmetric triangular decomposition

!      Inputs:
!         al(*)  - Lower terms in row
!         au(*)  - Upper terms in column
!         ad(*)  - Reduced diagonals of previous equations
!         jh     - Length of row/column

!      Outputs:
!         dured  - reduced diagonal for current equation
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: j,jh
      real (kind=8) :: dured, dot, al(jh),au(jh),ad(jh)

!     Scale upper U vector by reciprocal diagonals D

      do j = 1,jh
        au(j) = au(j)*ad(j)
      end do

!     Dot product of L * U

      dured = dot( al(1), au(1), jh)

!     Scale lower U vector by reciprocal diagonals D

      do j = 1,jh
        al(j) = al(j)*ad(j)
      end do

      end function dured

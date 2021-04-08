!$Id:$
      function dsred(au,ad,jh)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Reduce diagonal in symmetric triangular decomposition

!      Inputs:
!         au(*)  - Upper terms in column
!         ad(*)  - Reduced diagonals of previous equations
!         jh     - Length of column

!      Outputs:
!         dsred  - reduced diagonal for current equation
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: j,jh
      real (kind=8) :: dsred, ud, dj, au(jh),ad(jh)

      dj = 0.0d0
      do j = 1,jh
        ud    = au(j)*ad(j)
        dj    = dj + au(j)*ud
        au(j) = ud
      end do
      dsred = dj

      end function dsred

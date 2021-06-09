!$Id:$
      subroutine datest(au,jh,daval)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Check if equations are singular when zero diagonal
!               exists

!      Inputs:
!         au(*) - Column of A array
!         jh    - Height of column

!      Outputs:
!         daval - Sum of absolute values of column.

!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: j,jh
      real (kind=8) :: daval,au(jh)

      daval = 0.0d0
      do j = 1,jh
        daval = daval + abs(au(j))
      end do

      end subroutine datest

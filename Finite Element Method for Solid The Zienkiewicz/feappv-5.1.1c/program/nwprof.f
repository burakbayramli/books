!$Id:$
      subroutine nwprof(jp,neq)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Sum column heights to form profile pointer jp

!      Inputs:
!         jp(*)  - Column heights
!         neq    - Number of equations

!      Outputs:
!         jp(*)  - Profile pointer array
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: neq, n
      integer       :: jp(*)

      jp(1) = 0
      do n = 2,neq
        jp(n) = jp(n) + jp(n-1)
      end do

      end subroutine nwprof

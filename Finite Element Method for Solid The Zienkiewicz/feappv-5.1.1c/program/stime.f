!$Id:$
      subroutine stime()

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Initialize time clock

!      Inputs:
!         none

!      Outputs:
!         none      - Output is tim0 in common etime1
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'etime1.h'

      real (kind=4) :: etime, tt(2)

      save

      tim0 = 0.0d0
      tim0 = etime(tt)
      tim0 = tt(1)

      end

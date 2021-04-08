!$Id:$
      subroutine pltpt1(iel)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set 3-d Plot Sequence for 1-node point elements

!      Inputs:
!         iel       - Element type number

!      Outputs:
!         none      - Output through common block data
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'pdata5.h'
      include  'pdata6.h'

      integer       :: iel

      save

!     Set number of points

      if(iel.gt.0) then

        inord(iel)    = -1

      elseif(iel.lt.0) then

        exord(-iel)    = -1

      endif

      end subroutine pltpt1

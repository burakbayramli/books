!$Id:$
      subroutine pzero(v,nn)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Zero real array of data

!      Inputs:
!         nn     - Length of array

!      Outputs:
!         v(*)   - Array with zero values
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: nn
      real (kind=8) :: v(nn)

      save

      v(1:nn) = 0.0d0

      end subroutine pzero

!$Id:$
      subroutine pzeroi(ii,nn)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Zero integer array of data

!      Inputs:
!         nn     - Length of array

!      Outputs:
!         ii(*)  - Array with zero values
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: nn
      integer       :: ii(nn)

      save

!     Zero integer array
      ii(1:nn) = 0

      end subroutine pzeroi

!$Id:$
      subroutine pzerol(fl,val,nn)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set an array to logical value = val

!      Inputs:
!         val       - Logical value: true or false
!         nn        - Length of array to set

!      Outputs:
!         fl(*)     - Array set to logical state val
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer       :: n,nn
      logical       :: fl(nn),val

      save

      do n = 1,nn
        fl(n) = val
      end do

      end subroutine pzerol

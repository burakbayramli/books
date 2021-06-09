!$Id:$
      subroutine pmove(a,b,nn)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Move real array a into b

!      Inputs:
!         a(*)      - Array to move
!         nn        - Length of array to move

!      Outputs:
!         b(*)      - Moved array
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: n,nn
      real (kind=8) :: a(nn),b(nn)

      save

!     Move a-array into b-array

      do n = 1,nn
        b(n) = a(n)
      end do

      end subroutine pmove

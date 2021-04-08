!$Id:$
      subroutine pknotdiv(knotdiv,ii, kldiv)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Set knot parameters for shape functions

!      Inputs:
!         knotdiv(4,*) - Knot division array
!         ii           - Side number

!      Outputs:
!         kldiv(1)     - First knot number
!         kldiv(2)     - Last  knot number
!         kldiv(3)     - First control point
!         kldiv(4)     - Last  control point
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      integer (kind=4) :: i,ii, kldiv(4), knotdiv(4,*)

      do i = 1,4
        kldiv(i) = knotdiv(i,ii)
      end do ! i

      end subroutine pknotdiv

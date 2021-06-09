!$Id:$
      subroutine pushv3f (fi,v)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Compute push forward of contravariant vector

!     Inputs:
!        fi(3,3) - Deformation gradient
!        v(3)    - Contravariant vector

!     Outputs:
!        v(3)    - Pushed vector
!-----[--.----+----.----+----.-----------------------------------------]
      implicit none

      integer       :: i
      real (kind=8) :: fi(3,3),v(3),t(3)

!     Push-forward a 1 covariant vector.

      do i =1,3
        t(i) = v(1)*fi(1,i) + v(2)*fi(2,i) + v(3)*fi(3,i)
      end do ! i

      do i = 1,3
        v(i) = t(i)
      end do ! i

      end subroutine pushv3f

!$Id:$
      subroutine invert3(a, deta)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: 3 x 3 Matrix inverse

!      Inputs:
!        a(3,3)  - Matrix to invert

!      Outputs:
!        a(3,3)  - Inverse of matrix 'a'
!        deta    - Determinant of matrix 'a'
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      integer (kind=4) :: i, j
      real    (kind=8) :: a(3,3), ai(3,3), deta, deti

!     Compute determinant

      ai(1,1) = a(2,2)*a(3,3) - a(3,2)*a(2,3)
      ai(1,2) = a(3,2)*a(1,3) - a(1,2)*a(3,3)
      ai(1,3) = a(1,2)*a(2,3) - a(2,2)*a(1,3)

      deta = a(1,1)*ai(1,1) + a(2,1)*ai(1,2) + a(3,1)*ai(1,3)

!     Compute inverse

      deti    = 1.d0/deta
      ai(1,1) = ai(1,1)*deti
      ai(1,2) = ai(1,2)*deti
      ai(1,3) = ai(1,3)*deti

      ai(2,1) = (a(2,3)*a(3,1) - a(3,3)*a(2,1))*deti
      ai(2,2) = (a(3,3)*a(1,1) - a(1,3)*a(3,1))*deti
      ai(2,3) = (a(1,3)*a(2,1) - a(2,3)*a(1,1))*deti

      ai(3,1) = (a(2,1)*a(3,2) - a(3,1)*a(2,2))*deti
      ai(3,2) = (a(3,1)*a(1,2) - a(1,1)*a(3,2))*deti
      ai(3,3) = (a(1,1)*a(2,2) - a(2,1)*a(1,2))*deti

!     Store inverse back in original array

      do j = 1,3
        do i = 1,3
          a(i,j) = ai(i,j)
        end do ! i
      end do ! j

      end subroutine invert3

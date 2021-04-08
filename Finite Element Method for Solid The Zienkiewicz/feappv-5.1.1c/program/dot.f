!$Id:$
      function   dot(a,b,n)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: dot (scalar) product of two vectors

!      Inputs:
!         a(*)  - Vector 1
!         b(*)  - Vector 2
!         nn    - length of vectors

!      Outputs:
!         dot   - Scalar product
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: i,n
      real (kind=8) :: dot, a(*),b(*)

      dot = 0.0d0
      do i = 1,n
        dot = dot + a(i)*b(i)
      end do

      end function dot

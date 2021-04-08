!$Id:$
      function  dotx(a,b,n)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: dot (scalar) product of two vectors for (a_i - b_i)**2

!      Inputs:
!         a(*)  - Vector 1
!         b(*)  - Vector 2
!         nn    - length of vectors

!      Outputs:
!         dot   - Scalar product
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: i,n
      real (kind=8) :: dotx, a(*),b(*)

      dotx = 0.0d0
      do i = 1,n
        dotx = dotx + ( a(i) - b(i) )**2
      end do

      end function dotx

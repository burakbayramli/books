!$Id:$
      function dotid(a,b,id,nn)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: dot (scalar) product of two vectors using id array.

!      Inputs:
!         a(*)  - Vector 1
!         b(*)  - Vector 2 to be accessed using id array
!         id(*) - Equation pointer array
!         nn    - length of vectors

!      Outputs:
!         dotid - Scalar product
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: i,nn
      integer       :: id(*)
      real (kind=8) :: dotid, a(*),b(*)

      dotid = 0.d0
      do i = 1,nn
        if(id(i).gt.0) dotid = dotid + a(i)*b(id(i))
      end do

      end function dotid

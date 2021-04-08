!$Id:$
      logical function cknon0(v, nn )

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Check that real vector has a non-zero component

!      Inputs:
!         v(*)   - Vector of real numbers
!         nn     - Length of vector

!      Outputs:
!         cknon0 - true of non-zero entries exist; else false.
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: n,nn
      real (kind=8) :: v(*)

      do n = 1,nn
        if(v(n).ne.0.0d0) then
          cknon0 = .true.
          return
         endif
      end do ! n

!     Set false to indicate vector is zero

      cknon0 = .false.

      end function cknon0

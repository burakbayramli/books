!$Id:$
      logical function ckinon0(v, nn )

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Check that real vector has a non-zero component

!      Inputs:
!         v(*)   - Vector of integer numbers
!         nn     - Length of vector

!      Outputs:
!         cknon0 - true of non-zero entries exist; else false.
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer      :: n,nn, v(*)

      save

      do n = 1,nn
        if(v(n).ne.0) then
          ckinon0 = .true.
          return
         endif
      end do ! n

!     Set false to indicate vector is zero

      ckinon0 = .false.

      end function ckinon0

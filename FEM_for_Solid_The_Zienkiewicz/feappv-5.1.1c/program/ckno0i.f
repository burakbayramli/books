!$Id:$
      logical function ckno0i(iv, nn )

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Check that an integer vector has a non-zero component

!      Inputs:
!         iv(*)  - Vector of integers
!         nn     - Length of vector

!      Outputs:
!         ckno0i - true of non-zero entries exist; else false
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer      :: n,nn, iv(*)

      ckno0i = .false.
      do n = 1,nn
        if(iv(n).ne.0) then
          ckno0i = .true.
          return
         endif
      end do

      end function ckno0i

!$Id:$
      logical function pinput(d,nn)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Input routine for real data.: returns true on error

!      Inputs:
!         nn     - Number of data items to extract from input string
!                  N.B. Input performed by this function

!      Outputs:
!         d(*)   - Values of data input
!         pinput - Flag, returns true if error occurs during input
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      character (len=15) :: tl

      logical       :: tinput
      integer       :: nn
      real (kind=8) :: d(nn)

      save

!     Use routine TINPUT to perform the inputs

      pinput = tinput(tl,0,d,nn)

      end function pinput

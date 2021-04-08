!$Id:$
      logical function cksep(x1)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Check for existence of separator characters in data.
!               Separators are blank, comma, or equal sign.

!      Inputs:
!         x1  -  Character to check

!      Outputs:
!         cksep - True of character is a valid separator; else false.
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      character (len=1) :: x1

      cksep = (x1.eq.' ') .or. (x1.eq.',') .or. (x1.eq.'=')

      end function cksep

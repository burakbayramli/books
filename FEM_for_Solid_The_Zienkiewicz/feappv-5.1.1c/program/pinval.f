!$Id:$
      subroutine pinval(xs,val,error)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Moves character string into real value

!      Inputs:
!         xs(*)   - Character string

!      Outputs:
!         val     - Value extracted from character string
!         error   - Flag, true if error occurs
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      character (len=15) :: xs*15

      logical       :: error
      real (kind=8) :: val

      save

      read(xs,1000,err=100) val
      return
100   error = .true.

!     Format

1000  format(f15.0)

      end subroutine pinval

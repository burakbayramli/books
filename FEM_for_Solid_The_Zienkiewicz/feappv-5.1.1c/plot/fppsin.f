!$Id:$
      subroutine fppsin(string)

!     * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Stores string into buffer array in common /plplst/

!      Inputs:
!         string    - String of data to store

!      Outputs:
!         none
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'plpost.h'

      integer      :: i,l

      character    :: string*(*)

      save

!     Get length of string

      l = len(string)

!     Move string into buffer array

      if ((nxtchr+l) .ge. ibufsz) call fppsdu()
      do i = 1, l
        nxtchr         = nxtchr + 1
        buffer(nxtchr) = string(i:i)
      end do

      end subroutine fppsin

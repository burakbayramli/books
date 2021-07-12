c$Id:$
      subroutine fppsin(string)

c     * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Stores string into buffer array in common /plplst/

c      Inputs:
c         string    - String of data to store

c      Outputs:
c         none
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'plpost.h'

      integer   i,l

      character string*(*)

      save

c     Get length of string

      l = len(string)

c     Move string into buffer array

      if ((nxtchr+l) .ge. ibufsz) call fppsdu()
      do i = 1, l
        nxtchr         = nxtchr + 1
        buffer(nxtchr) = string(i:i)
      end do

      end

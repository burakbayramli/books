c$Id:$
      subroutine pinval(xs,val,error)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose:  Moves character string into real value

c      Inputs:
c         xs(*)   - Character string

c      Outputs:
c         val     - Value extracted from character string
c         error   - Flag, true if error occurs
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      logical   error
      character xs*15
      real*8    val

      save

      read(xs,1000,err=100) val
      return
100   error = .true.

c     Format

1000  format(f15.0)

      end

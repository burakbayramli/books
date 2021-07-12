c$Id:$
      subroutine pzero(v,nn)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Zero real array of data

c      Inputs:
c         nn     - Length of array

c      Outputs:
c         v(*)   - Array with zero values
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer   n,nn
      real*8    v(nn)

      save

      do n = 1,nn
        v(n) = 0.0d0
      end do

      end

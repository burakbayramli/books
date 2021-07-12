c$Id:$
      subroutine datest(au,jh,daval)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Check if equations are singular when zero diagonal
c               exists

c      Inputs:
c         au(*) - Column of A array
c         jh    - Height of column

c      Outputs:
c         daval - Sum of absolute values of column.

c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer   j,jh
      real*8    daval,au(jh)

      daval = 0.0d0
      do j = 1,jh
        daval = daval + abs(au(j))
      end do

      end

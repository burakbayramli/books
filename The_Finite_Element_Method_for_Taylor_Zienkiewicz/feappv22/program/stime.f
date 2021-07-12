c$Id:$
      subroutine stime()

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Initialize time clock

c      Inputs:
c         none

c      Outputs:
c         none      - Output is tim0 in common etime1
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'etime1.h'

      real*4    etime, tt(2)

      save

      tim0 = 0.0d0
      tim0 = etime(tt)
      tim0 = tt(1)

      end

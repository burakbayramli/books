c$Id:$
      function tand(x)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved
      implicit none

      real*8 tand, x

      tand = tan(atan(1.d0)*x/45.d0)

      end

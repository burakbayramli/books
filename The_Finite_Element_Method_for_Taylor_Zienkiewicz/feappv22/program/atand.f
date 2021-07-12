c$Id:$
      function atand(x)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved
      implicit none

      real*8 atand, x

      atand = 45.d0/atan(1.d0)*atan(x)

      end

c$Id:$
      function asind(x)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved
      implicit none

      real*8 asind, x

      asind = 45.d0/atan(1.d0)*asin(x)

      end

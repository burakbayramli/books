c$Id:$
      function psub(val)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

      implicit  none

      real*8    psub, val, xval

      save

      data      xval / 0.0d0 /

c     Look at parameter

      if(val.eq.0.0d0) then
        xval = 0.0d0
      else
        xval = xval - val
      endif

      psub = xval

      end

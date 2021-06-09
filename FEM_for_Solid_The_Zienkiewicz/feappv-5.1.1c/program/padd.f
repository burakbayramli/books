!$Id:$
      function padd(val)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

      implicit  none

      real (kind=8) :: padd, val, xval

      save

      data      xval /0.0d0/

!     Look at parameter

      if(val.eq.0.0d0) then
        xval = 0.0d0
      else
        xval = xval + val
      endif

      padd = xval

      end function padd

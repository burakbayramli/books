!$Id:$
      function tand(x)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved
      implicit none

      real (kind=8) :: tand, x

      tand = tan(atan(1.d0)*x/45.d0)

      end function tand

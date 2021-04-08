!$Id:$
      function sind(x)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved
      implicit none

      real (kind=8) :: sind, x

      sind = sin(atan(1.d0)*x/45.d0)

      end

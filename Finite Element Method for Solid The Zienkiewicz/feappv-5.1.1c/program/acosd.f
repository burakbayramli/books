!$Id:$
      function acosd(x)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved
      implicit none

      real (kind=8) :: acosd, x

      acosd = 45.d0/atan(1.d0)*acos(x)

      end function acosd

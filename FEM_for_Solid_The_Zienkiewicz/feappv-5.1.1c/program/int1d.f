!$Id:$
      subroutine int1d(l,sg)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Gauss quadrature for 1-d element

!      Inputs:
!         l     - Number of points

!      Outputs:
!         sg(2,*) - Gauss points & weights
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: l
      real (kind=8) :: sg(2,*), t

      if(l.eq.1) then

        sg(1,1) = 0.0d0
        sg(2,1) = 2.0d0

      elseif(l.eq.2) then

        sg(1,1) = -1.d0/sqrt(3.d0)
        sg(1,2) = -sg(1,1)
        sg(2,1) = 1.0d0
        sg(2,2) = 1.0d0

      elseif(l.eq.3) then

        sg(1,1) = -sqrt(0.6d0)
        sg(1,2) = 0.0d0
        sg(1,3) = -sg(1,1)
        sg(2,1) = 5.d0/9.d0
        sg(2,2) = 8.d0/9.d0
        sg(2,3) = sg(2,1)

      elseif(l.eq.4) then

        t       =  sqrt(4.8d0)
        sg(1,1) = -sqrt((3.d0+t)/7.d0)
        sg(1,2) = -sqrt((3.d0-t)/7.d0)
        sg(1,3) = -sg(1,2)
        sg(1,4) = -sg(1,1)
        t       =  1.d0/(3.d0*t)
        sg(2,1) =  0.5d0 - t
        sg(2,2) =  0.5d0 + t
        sg(2,3) =  sg(2,2)
        sg(2,4) =  sg(2,1)

      elseif(l.eq.5) then

        t       = sqrt(1120.0d0)

        sg(1,1) = (70.d0+t)/126.d0
        sg(1,2) = (70.d0-t)/126.d0

        t       = 1.d0/(15.d0 * (sg(1,2) - sg(1,1)))

        sg(2,1) = (5.0d0*sg(1,2) - 3.0d0)*t/sg(1,1)
        sg(2,2) = (3.0d0 - 5.0d0*sg(1,1))*t/sg(1,2)
        sg(2,3) =  2.0d0*(1.d0 - sg(2,1) - sg(2,2))
        sg(2,4) =  sg(2,2)
        sg(2,5) =  sg(2,1)

        sg(1,1) = -sqrt(sg(1,1))
        sg(1,2) = -sqrt(sg(1,2))
        sg(1,3) =  0.0d0
        sg(1,4) = -sg(1,2)
        sg(1,5) = -sg(1,1)

      endif

      end subroutine int1d

c$Id:$
      subroutine int1d(l,sg,wg)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Gauss quadrature for 1-d element

c      Inputs:
c         l     - Number of points

c      Outputs:
c         sg(*) - Gauss points
c         wg(*) - Gauss weights
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer   l
      real*8    sg(*),wg(*), t

      if(l.eq.1) then

        sg(1) = 0.0d0
        wg(1) = 2.0d0

      elseif(l.eq.2) then

        sg(1) = -1.d0/sqrt(3.d0)
        sg(2) = -sg(1)
        wg(1) = 1.0d0
        wg(2) = 1.0d0

      elseif(l.eq.3) then

        sg(1) = -sqrt(0.6d0)
        sg(2) = 0.0d0
        sg(3) = -sg(1)
        wg(1) = 5.d0/9.d0
        wg(2) = 8.d0/9.d0
        wg(3) = wg(1)

      elseif(l.eq.4) then

        t     =  sqrt(4.8d0)
        sg(1) = -sqrt((3.d0+t)/7.d0)
        sg(2) = -sqrt((3.d0-t)/7.d0)
        sg(3) = -sg(2)
        sg(4) = -sg(1)
        t     =  1.d0/(3.d0*t)
        wg(1) =  0.5d0 - t
        wg(2) =  0.5d0 + t
        wg(3) =  wg(2)
        wg(4) =  wg(1)

      elseif(l.eq.5) then

        t     = sqrt(1120.0d0)

        sg(1) = (70.d0+t)/126.d0
        sg(2) = (70.d0-t)/126.d0

        t     = 1.d0/(15.d0 * (sg(2) - sg(1)))

        wg(1) = (5.0d0*sg(2) - 3.0d0)*t/sg(1)
        wg(2) = (3.0d0 - 5.0d0*sg(1))*t/sg(2)
        wg(3) =  2.0d0*(1.d0 - wg(1) - wg(2))
        wg(4) =  wg(2)
        wg(5) =  wg(1)

        sg(1) = -sqrt(sg(1))
        sg(2) = -sqrt(sg(2))
        sg(3) =  0.0d0
        sg(4) = -sg(2)
        sg(5) = -sg(1)

      endif

      end

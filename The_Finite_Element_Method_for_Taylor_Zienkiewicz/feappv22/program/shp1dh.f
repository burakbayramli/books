c$Id:$
      subroutine shp1dh(sg,length, shpw,shpt)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c     Purpose: Shape functions for cubic hermitian functions on
c              straight line.

c     Inputs:
c        sg(2)      - Gauss point sg(1); and weight sg(2)
c        length     - Length of line segment

c     Outputs:
c       shpw(4,2)   - w-shape function/derivatives
c                     1 = N_w,x; 2 = N_w,xx 3 = N_w,xxx, 4 = N_w
c       shpt(4,2)   - theta-shape function/derivatives
c                     1 = N_t,x; 2 = N_t,xx 3 = N_t,xxx, 4 = t_w
c-----[--.----+----.----+----.-----------------------------------------]

      implicit    none

      real*8      length, xi,        xi2,        xi3
      real*8      sg(2),  shpw(4,2), shpt(4,2)

c     Powers of point

      xi  = sg(1)
      xi2 = xi*xi
      xi3 = xi*xi2

c     First derivatives (along length direction)

      shpw(1,1)  = - 1.5d0*(1.d0 - xi2)/length
      shpw(1,2)  = - shpw(1,1)

      shpt(1,1)  = 0.25d0*(- 1.d0 - 2.d0*xi + 3.d0*xi2)
      shpt(1,2)  = 0.25d0*(- 1.d0 + 2.d0*xi + 3.d0*xi2)

c     Second derivatives (along length direction)

      shpw(2,1)  =   6.0d0*xi/(length*length)
      shpw(2,2)  = - shpw(2,1)

      shpt(2,1)  = (- 1.d0 + 3.d0*xi)/length
      shpt(2,2)  = (  1.d0 + 3.d0*xi)/length

c     Third derivatives (along length direction)

      shpw(3,1)  =   12.0d0/(length**3)
      shpw(3,2)  = - shpw(3,1)

      shpt(3,1)  =   6.0d0/(length*length)
      shpt(3,2)  =   shpt(3,1)

c     Shape Functions

      shpw(4,1)  = 0.5d0 - 0.75d0*xi + 0.25d0*xi3
      shpw(4,2)  = 1.0d0 - shpw(4,1)

      shpt(4,1)  = 0.125d0*length*(  1.d0 - xi - xi2 + xi3)
      shpt(4,2)  = 0.125d0*length*(- 1.d0 - xi + xi2 + xi3)

      end

c$Id:$
      subroutine shap1d( xi, nel, shp )

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: One dimensional shape functions and natural derivatives

c      Inputs:
c         xi        - Isoparametric coordinate: ( -1 < xi < 1 )
c         nel       - Number of nodes / element   : ( 2 or 3 )

c      Outputs:
c         shp(2,3)  - Shape functions and spatial derivatives
c                     (natural derivatives only)
c         shp(1,i)  - Shape function spatial derivative: N_i,xi
c                     (natural derivatives only)
c         shp(2,i)  - Shape function                   : N_i
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer   nel
      real*8    xi, xi2

      real*8    shp(2,nel)

      save

c     2-node shape functions and derivatives

      if(nel.eq.2) then

        shp(1,1) = -0.5d0
        shp(1,2) =  0.5d0

        shp(2,1) =  0.5d0 - 0.5d0*xi
        shp(2,2) =  0.5d0 + 0.5d0*xi

c     3-node shape functions and derivatives

      elseif(nel.eq.3) then

        xi2      =  xi*xi

        shp(1,1) =  xi - 0.5d0
        shp(1,2) =  xi + 0.5d0
        shp(1,3) = -xi - xi

        shp(2,1) =  0.5d0*(xi2 - xi)
        shp(2,2) =  0.5d0*(xi2 + xi)
        shp(2,3) =  1.0d0 - xi2

      endif

      end

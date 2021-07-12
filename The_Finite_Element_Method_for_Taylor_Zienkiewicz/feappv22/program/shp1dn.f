c$Id:$
      subroutine shp1dn(s,shp,nel)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c     Purpose: Compute shape functions and natural derivatives
c              at natural coordinate s.
c              Linear (2 nodes) or quadratic (3 nodes) element.

c     Inputs:
c       s         : natural coordinate
c       nel       : number of nodes of element

c     Outputs:
c       shp(2,nel): shape functions and derivatives at s
c                   shp(1,1 to nel): derivatives of shape functions
c                   shp(2,1 to nel): shape functions
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer   nel
      real*8    s,shp(2,nel)

      save

c     Linear element

      if(nel.eq.2) then

        shp(1,1) = -0.5d0
        shp(1,2) =  0.5d0

        shp(2,1) =  0.5d0 - 0.5d0*s
        shp(2,2) =  0.5d0 + 0.5d0*s

c     Quadratic element

      elseif(nel.eq.3) then

        shp(1,1) =  s - 0.5d0
        shp(1,2) =  s + 0.5d0
        shp(1,3) = -2.d0*s

        shp(2,1) =  s*(s - 1.d0)*0.5d0
        shp(2,2) =  s*(s + 1.d0)*0.5d0
        shp(2,3) =  1.d0 - s*s

c     Cubic element

      elseif(nel.eq.4) then

        shp(1,1) = 0.0625d0*( 1.d0 + 18.d0*s - 27.d0*s*s)
        shp(1,2) = 0.0625d0*(-1.d0 + 18.d0*s + 27.d0*s*s)
        shp(1,3) = 0.5625d0*(-3.d0 - 2.d0*s + 9.d0*s*s)
        shp(1,4) = 0.5625d0*( 3.d0 - 2.d0*s - 9.d0*s*s)

        shp(2,1) = 0.0625d0*(9.d0*s*s - 1.d0)*(1.d0 - 3.d0*s)
        shp(2,2) = 0.0625d0*(9.d0*s*s - 1.d0)*(1.d0 + 3.d0*s)
        shp(2,3) = 0.5625d0*(1.d0 - s*s)*(1.d0 - 3.d0*s)
        shp(2,4) = 0.5625d0*(1.d0 - s*s)*(1.d0 + 3.d0*s)

      endif

      end

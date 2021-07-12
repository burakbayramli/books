c$Id:$
      subroutine cfunc(shp,xl,ixl,ndm,x)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Compute coordinates for point defined by local arrays.

c      Inputs:
c         shp(3,*)  - Shape function array
c         xl(ndm,*) - Array of element coordinates
c         ixl(*)    - Element node numbers
c         ndm       - Spatial dimension of mesh

c      Outputs:
c         x(ndm)    - Coordinates of point

c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer   k,l,ndm

      integer   ixl(*)
      real*8    shp(3,*),xl(3,*),x(*)

      do l = 1,ndm
        x(l) = 0.0d0
      end do

      do k = 1,9
        if(ixl(k).gt.0) then
          do l = 1,ndm
            x(l) = x(l) + shp(3,ixl(k))*xl(l,ixl(k))
          end do
        end if
      end do

      end

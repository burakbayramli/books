c$Id:$
      subroutine bjac3d ( rst , xl, ndm, shp, detj )

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Compute jacobian determinant and shape functions
c               with natural coord. derivatives for an 8-node brick.

c      Inputs:
c         rst(3)    - Natural coordinate location
c         xl(ndm,*) - Array of element coordinates
c         ndm       - Space dimension of mesh

c      Outputs:
c         shp(4,8)  - Shape functions and derivatives w/r natural coords.
c         detj      - Determinant of jacobian determinant
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer   ndm, i, j, k
      real*8    detj, xii, eti, zti

      real*8    rst(3), xl(ndm,*), shp(4,8), xs(3,3)
      real*8    xi(8), eta(8), zta(8)

      data xi /-0.5d0, 0.5d0, 0.5d0,-0.5d0,-0.5d0, 0.5d0, 0.5d0,-0.5d0/
      data eta/-0.5d0,-0.5d0, 0.5d0, 0.5d0,-0.5d0,-0.5d0, 0.5d0, 0.5d0/
      data zta/-0.5d0,-0.5d0,-0.5d0,-0.5d0, 0.5d0, 0.5d0, 0.5d0, 0.5d0/

c     Compute shape functions and derivatives

      do i = 1,8
        xii      = 0.5d0 +  xi(i)*rst(1)
        eti      = 0.5d0 + eta(i)*rst(2)
        zti      = 0.5d0 + zta(i)*rst(3)
        shp(1,i) = eti*zti*xi(i)
        shp(2,i) = xii*zti*eta(i)
        shp(3,i) = xii*eti*zta(i)
        shp(4,i) = xii*eti*zti
      end do

c     Compute jacobian matrix

      do i = 1,3
        do j = 1,3
          xs(i,j) = 0.0d0
          do k = 1,8
            xs(i,j) = xs(i,j) + xl(i,k)*shp(j,k)
          end do
        end do
      end do

c     Compute jacobian determinant

      detj = xs(1,1)*(xs(2,2)*xs(3,3) - xs(2,3)*xs(3,2))
     &     + xs(1,2)*(xs(2,3)*xs(3,1) - xs(2,1)*xs(3,3))
     &     + xs(1,3)*(xs(2,1)*xs(3,2) - xs(2,2)*xs(3,1))

      end

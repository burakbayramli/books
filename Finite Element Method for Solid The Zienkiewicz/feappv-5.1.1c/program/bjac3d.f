!$Id:$
      subroutine bjac3d ( rst , xl, ndm, shp, detj )

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute jacobian determinant and shape functions
!               with natural coord. derivatives for an 8-node brick.

!      Inputs:
!         rst(3)    - Natural coordinate location
!         xl(ndm,*) - Array of element coordinates
!         ndm       - Space dimension of mesh

!      Outputs:
!         shp(4,8)  - Shape functions and derivatives w/r natural coords.
!         detj      - Determinant of jacobian determinant
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer       :: ndm, i, j, k
      real (kind=8) :: detj, xii, eti, zti

      real (kind=8) :: rst(3), xl(ndm,*), shp(4,8), xs(3,3)
      real (kind=8) :: xi(8), eta(8), zta(8)

      data xi /-0.5d0, 0.5d0, 0.5d0,-0.5d0,-0.5d0, 0.5d0, 0.5d0,-0.5d0/
      data eta/-0.5d0,-0.5d0, 0.5d0, 0.5d0,-0.5d0,-0.5d0, 0.5d0, 0.5d0/
      data zta/-0.5d0,-0.5d0,-0.5d0,-0.5d0, 0.5d0, 0.5d0, 0.5d0, 0.5d0/

!     Compute shape functions and derivatives

      do i = 1,8
        xii      = 0.5d0 +  xi(i)*rst(1)
        eti      = 0.5d0 + eta(i)*rst(2)
        zti      = 0.5d0 + zta(i)*rst(3)
        shp(1,i) = eti*zti*xi(i)
        shp(2,i) = xii*zti*eta(i)
        shp(3,i) = xii*eti*zta(i)
        shp(4,i) = xii*eti*zti
      end do

!     Compute jacobian matrix

      do i = 1,3
        do j = 1,3
          xs(i,j) = 0.0d0
          do k = 1,8
            xs(i,j) = xs(i,j) + xl(i,k)*shp(j,k)
          end do
        end do
      end do

!     Compute jacobian determinant

      detj = xs(1,1)*(xs(2,2)*xs(3,3) - xs(2,3)*xs(3,2))
     &     + xs(1,2)*(xs(2,3)*xs(3,1) - xs(2,1)*xs(3,3))
     &     + xs(1,3)*(xs(2,1)*xs(3,2) - xs(2,2)*xs(3,1))

      end subroutine bjac3d

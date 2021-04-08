!$Id:$
      subroutine bjac2d ( rst , xl, ndm, shp, detj )

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute jacobian determinant and shape functions
!               with natural derivatives for an 4-node quadrilateral.

!      Inputs:
!         rst(2)    - Natural coordinate location
!         xl(ndm,*) - Array of element coordinates
!         ndm       - Space dimension of mesh

!      Outputs:
!         shp(2,4)  - Shape functions and derivatives w/r natural coords.
!         detj      - Determinant of jacobian determinant
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: ndm, i, j, k
      real (kind=8) :: detj, xii, eti

      real (kind=8) :: rst(2), xl(ndm,*), shp(3,4),xs(2,2),xi(4),eta(4)

      data      xi /-0.5d0, 0.5d0, 0.5d0,-0.5d0/
      data      eta/-0.5d0,-0.5d0, 0.5d0, 0.5d0/

!     Compute shape functions and derivatives

      do i = 1,4
        xii      = 0.5d0 +  xi(i)*rst(1)
        eti      = 0.5d0 + eta(i)*rst(2)
        shp(1,i) = eti*xi(i)
        shp(2,i) = xii*eta(i)
        shp(3,i) = xii*eti
      end do

!     Compute jacobian matrix

      do i = 1,2
        do j = 1,2
          xs(i,j) = 0.0d0
          do k = 1,4
            xs(i,j) = xs(i,j) + xl(i,k)*shp(j,k)
          end do
        end do
      end do

!     Compute jacobian determinant

      detj = xs(1,1)*xs(2,2) - xs(1,2)*xs(2,1)

      end subroutine bjac2d

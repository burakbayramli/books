!$Id:$
      subroutine bmat2d(c,r,shp,g,bbar)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Sets B-bar matrix for 2-d problems

!      Inputs:
!         c         - Constant for plane = 0; for axisymm = 1
!         r         - Radius for axisymmetrix (= 1 for plane)
!         shp(3)    - Shape function and derivatives
!         g(2)      - b-bar integrals

!      Outputs:
!         bbar(4,2) - B-bar matrix for a node
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      real (kind=8) :: c,r,bb1,bb2,sh3
      real (kind=8) :: shp(3),g(2),bbar(4,2)

      save

!     Mixed modification to form B-bar

      sh3 = c*shp(3)/r
      bb1 = (g(1) - shp(1) - sh3)*0.3333333333333333d0
      bb2 = (g(2) - shp(2)      )*0.3333333333333333d0

!     B-bar matrix for plane and axisymmetric problems

      bbar(1,1) = bb1 + shp(1)
      bbar(2,1) = bb1
      bbar(3,1) = bb1 + sh3
      bbar(4,1) = shp(2)
      bbar(1,2) = bb2
      bbar(2,2) = bb2 + shp(2)
      bbar(3,2) = bb2
      bbar(4,2) = shp(1)

      end subroutine bmat2d

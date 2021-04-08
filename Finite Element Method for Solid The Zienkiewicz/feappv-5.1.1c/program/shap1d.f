!$Id:$
      subroutine shap1d( xi, nel, shp )

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: One dimensional shape functions & natural derivatives

!      Inputs:
!         xi        - Isoparametric coordinate: ( -1 < xi < 1 )
!         nel       - Number of nodes / element   : ( 2 or 3 )

!      Outputs:
!         shp(2,3)  - Shape functions & spatial derivatives
!                     (natural derivatives only)
!         shp(1,i)  - Shape function spatial derivative: N_i,xi
!                     (natural derivatives only)
!         shp(2,i)  - Shape function                   : N_i
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: nel
      real (kind=8) :: xi, xi2
      real (kind=8) :: shp(2,nel)

      save

!     2-node shape functions & derivatives
      if(nel.eq.2) then

        shp(1,1) = -0.5d0
        shp(1,2) =  0.5d0

        shp(2,1) =  0.5d0 - 0.5d0*xi
        shp(2,2) =  0.5d0 + 0.5d0*xi

!     3-node shape functions & derivatives
      elseif(nel.eq.3) then

        shp(1,1) =  xi - 0.5d0
        shp(1,2) =  xi + 0.5d0
        shp(1,3) = -xi - xi

        xi2      =  xi*xi
        shp(2,1) =  0.5d0*(xi2 - xi)
        shp(2,2) =  0.5d0*(xi2 + xi)
        shp(2,3) =  1.0d0 - xi2

      endif

      end subroutine shap1d

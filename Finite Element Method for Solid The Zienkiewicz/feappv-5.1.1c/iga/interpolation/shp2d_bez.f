!$Id:$
      subroutine shp2d_bez(sg,wb, p,q, ic, shp)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Compute shape functions for 2-d Bezier polynomials

!     Inputs:
!       sg(2)            - Quadrature point
!       wb(*)            - Bezier projection weights
!       p,r              - Nurb order in each direction

!     Outputs:
!       ic               - Number of Bezier 2-d functions
!       shp(*)           - Shape functions
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'eldata.h'
      include   'iofile.h'

!     --------------VARIABLE DECLARATIONS-----------------------------
      integer (kind=4) :: p,q, ic
      real    (kind=8) :: sg(2), wb(*), shp(3,*)

!     1D non-rational basis functions and derivatives
      real    (kind=8) :: Bshp1(3,6), Bshp2(3,6), ww(3)

!     NURBS counters for loops
      integer (kind=4) :: i, j

! ------------------------------------------------------------------
!     Set NURBS coordinate data

      if(nel.gt.36) then
        write(iow,*) ' Number of nodes on element too large'
        call plstop(.true.)
      endif

!     Evaluate 1D shape functions and derivatives each direction

      call bezier1d(sg(1), p, Bshp1)
      call bezier1d(sg(2), q, Bshp2)

!     Form local Bezier basis functions and derivatives

      ic = 0
      do j = 0,q
        do i = 0,p
          ic = ic+1
          shp(1,ic) = Bshp1(2,i+1)*Bshp2(1,j+1)   ! xi_1 derivative
          shp(2,ic) = Bshp1(1,i+1)*Bshp2(2,j+1)   ! xi_2 derivative
          shp(3,ic) = Bshp1(1,i+1)*Bshp2(1,j+1)   ! shape function
        end do ! i
      end do ! j

!     Form rational Bezier basis

      do j = 1,3
        ww(j) = 0.0d0
        do i = 1,ic
          shp(j,i) = shp(j,i)*wb(i)
          ww(j)    = ww(j) + shp(j,i)
        end do ! i
      end do ! j

!     Form final function and first derivatives

      ww(3) = 1.d0/ww(3)
      ww(1) = ww(1)*ww(3)
      ww(2) = ww(2)*ww(3)
      do i = 1,ic
        shp(1,i) = (shp(1,i) - shp(3,i)*ww(1))*ww(3)
        shp(2,i) = (shp(2,i) - shp(3,i)*ww(2))*ww(3)
        shp(3,i) =  shp(3,i) * ww(3)
      end do ! i

      end subroutine shp2d_bez

!$Id:$
      subroutine shp3d_bez(sg,wb, p,q,r, ic, shp)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Compute shape functions for 3-d Bezier polynomials

!     Inputs:
!       sg(3)            - Quadrature point
!       wb(*)            - Bezier projection weights
!       p,q,r            - Nurb order in each direction

!     Outputs:
!       ic               - Number of Bezier 3-d functions
!       shp(*)           - Shape functions
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'eldata.h'
      include   'iofile.h'

!     --------------VARIABLE DECLARATIONS-----------------------------
      integer (kind=4) :: p,q,r, ic
      real    (kind=8) :: sg(3), wb(*), shp(4,*)

!     1D non-rational basis functions and derivatives
      real    (kind=8) :: Bshp1(3,6), Bshp2(3,6), Bshp3(3,6), ww(4)

!     NURBS counters for loops
      integer (kind=4) :: i, j, k

! ------------------------------------------------------------------
!     Set NURBS coordinate data

      if(nel.gt.64) then
        write(iow,*) ' Number of nodes on element',n_el,' too large'
        call plstop(.true.)
      endif

!     Evaluate 1D shape functions and derivatives each direction

      call bezier1d(sg(1), p, Bshp1)
      call bezier1d(sg(2), q, Bshp2)
      call bezier1d(sg(3), r, Bshp3)

!     Form local Bezier basis functions and derivatives

      ic = 0
      do k = 1,r+1
        do j = 1,q+1
          do i = 1,p+1
            ic        = ic+1
            shp(1,ic) = Bshp1(2,i)*Bshp2(1,j)*Bshp3(1,k)  ! xi_1 derivs
            shp(2,ic) = Bshp1(1,i)*Bshp2(2,j)*Bshp3(1,k)  ! xi_2 derivs
            shp(3,ic) = Bshp1(1,i)*Bshp2(1,j)*Bshp3(2,k)  ! xi_3 derivs
            shp(4,ic) = Bshp1(1,i)*Bshp2(1,j)*Bshp3(1,k)  ! shape funcs
          end do ! i
        end do ! j
      end do ! k

!     Form rational Bezier basis

      do j = 1,4
        ww(j) = 0.0d0
        do i = 1,ic
          shp(j,i) = shp(j,i)*wb(i)
          ww(j)    = ww(j) + shp(j,i)
        end do ! i
      end do ! j

!     Form final function and first derivatives

      ww(4) = 1.d0/ww(4)
      ww(1) = ww(1)*ww(4)
      ww(2) = ww(2)*ww(4)
      ww(3) = ww(3)*ww(4)
      do i = 1,ic
        shp(1,i) = (shp(1,i) - shp(4,i)*ww(1))*ww(4)
        shp(2,i) = (shp(2,i) - shp(4,i)*ww(2))*ww(4)
        shp(3,i) = (shp(3,i) - shp(4,i)*ww(3))*ww(4)
        shp(4,i) =  shp(4,i) * ww(4)
      end do ! i

      end subroutine shp3d_bez

!$Id:$
      subroutine derbezier1d(sg, c_e, p, is, Nshp)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute Bernstein polynomials and derivatives (1 & 2)
!               Multiply by extraction operator fo form NURBS functions

!      Inputs:
!        sg        -- Quadrature point
!        c_e(*)    -- Extraction operator
!        p         -- Polynomial order
!        is        -- Span of extraction operator

!      Output:
!        Nshp(4,*) -- Shape functions: 1 = shp function.
!                                      2 = der-1; 3 = der-2; 4 = der-3
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      integer (kind=4) :: p,is, i,j,k
      real    (kind=8) :: sg
      real    (kind=8) :: c_e(p+1,p+1,*), Nshp(4,*), Bshp(3,20)

!     Compute Bernstein polynomials

      call bezier1d(sg,p, Bshp)

!     Multiply by extraction operator

      do i = 1,3
        do j = 1,p+1
          Nshp(i,j) = 0.0d0
          do k = 1,p+1
            Nshp(i,j) = Nshp(i,j) + Bshp(i,k)*C_e(k,j,is)
          end do ! k
        end do ! j
      end do ! i

      end subroutine derbezier1d

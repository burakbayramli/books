!$Id:$
      subroutine mixbezier1d(sg, c_e, p, is, Ishp)

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
!        Ishp(*)   -- Shape functions
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'iofile.h'

      integer (kind=4) :: p,is, j,k
      real    (kind=8) :: sg
      real    (kind=8) :: c_e(p+1,p+1,*), Ishp(*), Bshp(20)

!     Compute Bernstein polynomials

      call beziers(sg,p, Bshp)

!     Multiply by extraction operator

      do j = 1,p+1
        Ishp(j) = 0.0d0
        do k = 1,p+1
          Ishp(j) = Ishp(j) + Bshp(k)*C_e(k,j,is)
        end do ! k
      end do ! j

      end subroutine mixbezier1d

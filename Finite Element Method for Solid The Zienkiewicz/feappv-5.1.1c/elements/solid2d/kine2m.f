!$Id:$
      subroutine kine2m(shp,ul,f,df,finv,ndf,nel,nen,detf,lint)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Compute deformation gradient and its inverse at tn+1

!     Inputs:
!        shp(3,9,*)  - Shape functions and derivatives at gauss points
!        ul(ndf,*)   - Nodal solution parameters
!        ul(2,*)     - Nodal stress free reference displacements
!        ndf         - Number dof/node
!        nel         - Number nodes/element
!        nen         - Maximum number nodes/element
!        lint        - Number of quadrature points

!     Outputs:
!        f(9,*)      - Deformation gradient at gauss points
!        df(9,*)     - Incremental deformation gradient at points
!        finv(9,*)   - Incremental deformation gradient at points
!        detf(*)     - Determinant of deformation gradient at points
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer       :: ndf, nel,nen, lint, k, l
      real (kind=8) :: shp(3,64,*),ul(ndf,nen,*)
      real (kind=8) :: df(9,*),f(9,2,*),detf(2,*),finv(9,*),dfi(9)

      save

!     Compute deformation gradient at t-n+1
!     F = I + Grad u
      do l = 1,lint
        f(:,:,l) = 0.0d0
        do k = 1,nel
          f(1,1,l) = f(1,1,l) + ul(1,k,1)*shp(1,k,l)
          f(2,1,l) = f(2,1,l) + ul(2,k,1)*shp(1,k,l)
          f(4,1,l) = f(4,1,l) + ul(1,k,1)*shp(2,k,l)
          f(5,1,l) = f(5,1,l) + ul(2,k,1)*shp(2,k,l)
        end do ! k
        f(1,1,l) = f(1,1,l) + 1.0d0
        f(5,1,l) = f(5,1,l) + 1.0d0
        f(9,1,l) = f(9,1,l) + 1.0d0

!       Det F
        detf(1,l) = f(1,1,l)*f(5,1,l)-f(2,1,l)*f(4,1,l)

!       F^{-1}
        finv(1,l) = f(5,1,l)/detf(1,l)
        finv(2,l) =-f(2,1,l)/detf(1,l)
        finv(3,l) = 0.0d0
        finv(4,l) =-f(4,1,l)/detf(1,l)
        finv(5,l) = f(1,1,l)/detf(1,l)
        finv(6,l) = 0.0d0
        finv(7,l) = 0.0d0
        finv(8,l) = 0.0d0
        finv(9,l) =  1.0d0

!       Compute incremental deformation gradient
        dfi(:) = 0.0d0
        do k = 1,nel
          dfi(1) = dfi(1) + ul(1,k,2)*shp(1,k,l)
          dfi(2) = dfi(2) + ul(2,k,2)*shp(1,k,l)
          dfi(4) = dfi(4) + ul(1,k,2)*shp(2,k,l)
          dfi(5) = dfi(5) + ul(2,k,2)*shp(2,k,l)
        end do

!       Compute deformation gradient F_n
        f(:,2,l) = f(:,1,l) - df(:,l)
        detf(2,l) = f(1,2,l)*f(5,2,l) - f(2,2,l)*f(4,2,l)

      end do ! l

      end subroutine kine2m

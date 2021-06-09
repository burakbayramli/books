!$Id:$
      subroutine slcn1d(sig,eps,shp,xsj,p,s,lint,nel,nes)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Project element variables to nodes

!      Inputs:
!        sig(nes,*) - Stresses at quadrature points
!        eps(3,*)   - Strains  at quadrature points
!        shp(2,8,*) - Shape functions at quadrature points
!        xsj(*)     - Volume element at quadrature points
!        lint       - Number of quadrature points
!        nel        - Number nodes on element
!        nes        - Dimension of stress array

!      Outputs:
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'strnum.h'

      integer       :: nel,nes, i,l,lint
      real (kind=8) :: p(*),s(nen,*),xsj(*) ,sig(nes,*),eps(3,*)
      real (kind=8) :: shp(2,20,*), xg

      save

!     Lumped and consistent projection routine

      do l = 1,lint

!       Compute lumped projection and assemble stress integrals

        do i = 1,nel

          xg     = shp(2,i,l)*xsj(l)
          p(i) = p(i) + xg

!         Stress projections

          s(i,1) = s(i,1) + sig(1,l)*xg
          s(i,2) = s(i,2) + sig(2,l)*xg
          s(i,3) = s(i,3) + sig(3,l)*xg

!         Strain projections

          s(i,4) = s(i,4) + eps(1,l)*xg
          s(i,5) = s(i,5) + eps(2,l)*xg
          s(i,6) = s(i,6) + eps(3,l)*xg

        end do ! i
      end do ! l

      iste = 6

      end subroutine slcn1d

!$Id:$
      subroutine slcn2d(sig,p,s,se,nel,nes)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Project element variables to nodes

!      Inputs:
!        sig(nes,*) - Stresses at quadrature points
!        nel        - Number nodes on element
!        nes        - Dimension of stress array

!      Outputs:
!        p(*)       - Weights for 'lumped' projection
!        s(nen,*)   - Integral of variables
!        se(nen)    - Error projectors
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'prstrs.h'
      include  'qudshp.h'
      include  'strnum.h'

      integer       :: nel,nes, i,l
      real (kind=8) :: p(*),s(nen,*),se(*),sig(nes,*), xg

      save

!     FEA lumped projection routine

      do l = 1,lint
        do i = 1,nel

!         Stress projections
          xg       = shp2(3,i,l)*jac(l)
          p(i)     = p(i) + xg
          s(i,1:4) = s(i,1:4) + sig(1:4,l)*xg

!         Error estimation projection

          se(i)  = se(i)  + erav*xg
        end do ! i
      end do ! l

      iste = 4

      end subroutine slcn2d

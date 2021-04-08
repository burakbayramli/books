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
!        p(nen)     - Weights for 'lumped' projection
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

!     VEM projections

      if(vemfl) then
        call vem_slcn(sig,p,s,nes)

!     FEA projection routine

      else

        do l = 1,lint
          do i = 1,nel
            xg   = shp2(3,i,l)*jac(l)
            p(i) = p(i) + xg

!           Stress projections

            s(i,1) = s(i,1) + sig(1,l)*xg
            s(i,2) = s(i,2) + sig(2,l)*xg
            s(i,3) = s(i,3) + sig(3,l)*xg
            s(i,4) = s(i,4) + sig(4,l)*xg

!           Error estimation projection

            se(i)  = se(i)  + erav*xg

          end do ! i
        end do ! l

      endif

      iste = 4

      end subroutine slcn2d

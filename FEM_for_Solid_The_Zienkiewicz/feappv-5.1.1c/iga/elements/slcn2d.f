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

      integer   nel,nes, i,l
      real*8    xg

      real*8    p(*),s(nen,*),se(*),sig(nes,*)

      integer   j
      real*8    pmat(64,64),psig(4,64)
      real*8    dfac,cfac

      save

      if(nurbfl) then

        do j = 1,nel
          do i = 1,nel
            pmat(i,j) = 0.0d0
          end do ! i
          do i = 1,4
            psig(i,j) = 0.0d0
          end do ! i
        end do ! j

        dfac = 0.25d0
        cfac = 1.0d0 - dfac

        do l = 1,lint
          do j = 1,nel
            xg = shp2(3,j,l)*jac(l)
            do i = 1,nel
              pmat(i,j) = pmat(i,j) + cfac*shp2(3,i,l)*xg
            end do ! i
            pmat(j,j) = pmat(j,j) + dfac*xg
            do i = 1,4
              psig(i,j) = psig(i,j) + xg*sig(i,l)
            end do ! i
          end do ! j
        end do ! l
        call invert(pmat,nel,64)
        do j = 1,nel
          do i = 1,4
            do l = 1,nel
              s(j,i) = s(j,i) +  pmat(j,l)*psig(i,l)
            end do ! l
          end do ! i
!         p(j) = p(j) + 1.0d0
          p(j) = 1.0d0
        end do ! j

!     Standard lumped projection routine

      else
        do l = 1,lint

!         Compute lumped projection and assemble stress integrals

          do i = 1,nel

            xg     = shp2(3,i,l)*jac(l)
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

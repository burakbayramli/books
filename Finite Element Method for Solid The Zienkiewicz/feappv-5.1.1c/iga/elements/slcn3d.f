!$Id:$
      subroutine slcn3d(sig,p,s,nel,nes)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Project element variables to nodes

!      Inputs:
!        sig(nes,*)   - Stresses at quadrature points
!        lint         - Number of quadrature points
!        nel          - Number nodes on element

!      Outputs:
!        p(nen)       - Weights for 'lumped' projection
!        s(nen,*)     - Integral of variables
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'qudshp.h'
      include  'strnum.h'

      integer   i, j, l, nel, nes
      real*8    dfac,cfac, xg

      real*8    sig(nes,*), p(*), s(nen,*)
      real*8    pmat(125,125),psig(6,125)

      save

      if(nurbfl) then

        do j = 1,nel
          do i = 1,nel
            pmat(i,j) = 0.0d0
          end do ! i
          do i = 1,6
            psig(i,j) = 0.0d0
          end do ! i
        end do ! j

        dfac = 0.25d0
        cfac = 1.0d0 - dfac

        do l = 1,lint
          do j = 1,nel
            xg = shp3(4,j,l)*jac(l)
            do i = 1,nel
              pmat(i,j) = pmat(i,j) + cfac*shp3(4,i,l)*xg
            end do ! i
            pmat(j,j) = pmat(j,j) + dfac*xg
            do i = 1,6
              psig(i,j) = psig(i,j) + xg*sig(i,l)
            end do ! i
          end do ! j
        end do ! l
        call invert(pmat,nel,125)
        do j = 1,nel

          do i = 1,6
            do l = 1,nel
              s(j,i) = s(j,i) +  pmat(j,l)*psig(i,l)
            end do ! l
          end do ! i
          p(j) = p(j) + 1.0d0
        end do ! j

!     Standard lumped projection routine

      else
        do l = 1,lint

!         Compute lumped projection and assemble stress integrals

          do i = 1,nel
            xg   = shp3(4,i,l)*jac(l)
            p(j) = p(j) + xg

!           Stress projections
            do j = 1,6
              s(j,j) = s(j,j) + sig(j,l)*xg
            end do ! j

          end do ! i
        end do ! l

      endif

      iste = 6

      end

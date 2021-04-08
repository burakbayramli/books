!$Id:$
      subroutine slcn3d(sig, p,s, nel,nes)

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Project element variables to nodes

!      Inputs:
!        sig(nes,*)  - Stresses at quadrature points
!        nel          - Number nodes on element
!        nes          - Dimension of shape function array

!      Outputs:
!        p(nen)       - Weights for 'lumped' projection
!        s(nen,*)     - Integral of variables
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'qudshp.h'
      include  'strnum.h'

      integer       :: ii, jj, l, nel,nes
      real (kind=8) :: p(*),s(nen,*), sig(nes,*), xj

      save

!     Initialize the arrays

      do ii = 1,nel
        p(ii)    = 0.0d0
        do jj = 1,6
          s(ii,jj) = 0.0d0
        end do ! jj
      end do ! ii

!     Compute projections: int ( sig * shp(i) * darea )

      do l = 1,lint
        do ii = 1,nel
          xj    = jac(l)*shp3(4,ii,l)
          p(ii) = p(ii)   + xj
          do jj = 1,6
            s(ii,jj) = s(ii,jj) + sig(jj,l)*xj
          end do ! jj
        end do ! ii
      end do ! l

      end subroutine slcn3d

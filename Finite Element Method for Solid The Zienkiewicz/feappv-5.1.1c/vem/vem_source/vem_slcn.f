!$Id:$
      subroutine vem_slcn(sig,p,s,nes)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: 2-D VEM: Projection of displacements and stresses

!     Inputs:
!       sig(new,:)    - Stress at nodes
!       nen           - Maximum nodes/element

!     Outputs:
!       s(numnp,*)    - Projection of values
!       p(*)          - Diagonal projection matrix
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cdata.h'
      include   'eldata.h'
      include   'strnum.h'
      include   'pconstant.h'
      include   'qudshp.h'
      include   'vem_data.h'

      integer          :: nes, i,j,c, ne, l, ll, ii,jj
      real    (kind=8) :: sig(nes,*)
      real    (kind=8) :: p(*), s(numnp,*)
      real    (kind=8) :: mat(3,3),xg(3)
      real    (kind=8) :: sigg(4,3)
      real    (kind=8) :: gsig(4,3)

      save

      select case (k_order)

      case (1)

        do i = 1,nel
          p(i)     = p(i)     + 1.0d0
          s(i,1:4) = s(i,1:4) + sig(1:4,1)
        end do ! i

      case (2)

        ll = 0
        ne = nel/2
        do ii = 1,ne

          jj = mod(ii,ne) + 1
          c  = ne + ii

          sigg(:,:) = 0.0d0
          mat(:,:)  = 0.0d0
          do l = 1,lintv(ii)
            ll    = ll + 1
            xg(:) = elv(1:3,ll)*elv(4,ll)
            do j = 1,3  ! WIP was 1,6
              mat(j,:)  = mat(j,:)  + elv(j,ll)  *xg(:)
              sigg(:,j) = sigg(:,j) + sig(1:4,ll)*xg(j)
            end do ! j
          end do ! l
          call invert(mat,3,3)
          do j = 1,3
            gsig(:,j) = mat(j,1)*sigg(:,1)
     &                + mat(j,2)*sigg(:,2)
     &                + mat(j,3)*sigg(:,3)
          end do ! j

          s(ii,1:4)  = s(ii,1:4) + gsig(1:4,1)
          s(jj,1:4)  = s(jj,1:4) + gsig(1:4,2)
          s(nel,1:4) = s(nel,1:4)+ gsig(1:4,3)
          s(c,1:4)   = s(c,1:4)  + (gsig(1:4,1)
     &                                    + gsig(1:4,2))*0.5d0

!         Compute weights for plots

          p(ii)       = p(ii)  + 1.0d0
          p(jj)       = p(jj)  + 1.0d0
          p(c )       = p(c )  + 1.0d0
          p(nel)      = p(nel) + 1.0d0

        end do ! i

      case default

        write(*,*) ' Not available in vFEAPpv'

      end select

      iste = 4

      end subroutine vem_slcn

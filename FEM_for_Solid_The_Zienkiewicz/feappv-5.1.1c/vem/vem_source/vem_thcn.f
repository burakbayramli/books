!$Id:$
      subroutine vem_thcn(flux,p,s)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    26/07/2017
!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: 2-D VEM: Projection of thermal flux

!     Inputs:
!       flux(2,*)     - Flux values

!     Inputs: From include 'qudshp.h'
!       flux(3,:)     - Flux  points

!     Outputs:
!       s(nen,*)      - Projection of values
!       p(*)          - Diagonal projection matrix
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cdata.h'
      include   'eldata.h'
      include   'strnum.h'
      include   'pconstant.h'
      include   'qudshp.h'
      include   'vem_data.h'

      integer          :: i,j,c, ne, l, ll, ii,jj
      real    (kind=8) :: p(*), s(nen,*)
      real    (kind=8) :: mat(3,3),xg(3)
      real    (kind=8) :: flux(2,*), fluxg(2,3)
      real    (kind=8) :: gflux(2,3)

      save

      select case (k_order)

      case (1)

        do i = 1,nel
          p(i)     = p(i)     + 1.0d0
          s(i,1:2) = s(i,1:2) + flux(1:2,1)
        end do ! i

      case (2)

        mat(:,:) = 0.0d0
        do l = 1,lint2
          xg(:) = el2(1:3,l)*el2(4,l)
          do i = 1,3
            mat(:,i) = mat(:,i) + xg(:)*el2(i,l)
          end do ! i
        end do ! l

        call invert(mat,3,3)

        ll = 0
        ne = nel/2
        do ii = 1,ne
          jj = mod(ii,ne) + 1
          c  = ne + ii

          fluxg(:,:) = 0.0d0
          do l = 1,lintv(ii)
            ll    = ll + 1
            xg(:) = elv(1:3,ll)*elv(4,ll)
            do j = 1,3
              fluxg(:,j) = fluxg(:,j) + flux(:,l+ll)*xg(j)
            end do ! j
          end do ! l
          do j = 1,3
            gflux(:,j) = mat(j,1)*fluxg(:,1)
     &                 + mat(j,2)*fluxg(:,2)
     &                 + mat(j,3)*fluxg(:,3)
          end do ! j

          s(ii ,1:2) = s(ii ,1:2) + gflux(1:2,1)
          s(jj ,1:2) = s(jj ,1:2) + gflux(1:2,2)
          s(nel,1:2) = s(nel,1:2) + gflux(1:2,3)
          s(c  ,1:2) = s(c  ,1:2) + (gflux(1:2,1)
     &                            +  gflux(1:2,2))*0.5d0

!         Compute weights for plots

          p(ii ) = p(ii ) + 1.0d0
          p(jj ) = p(jj ) + 1.0d0
          p(c  ) = p(c  ) + 1.0d0
          p(nel) = p(nel) + 1.0d0

        end do ! i

      case default

      end select

      iste = 2

      end subroutine vem_thcn

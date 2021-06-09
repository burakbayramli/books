!$Id:$
      subroutine vem_stab2d(trS, ul, s,r, ndf,nel,nst, nds, isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: 2-D VEM: Virtual Element Method: 2-d Stabilization

!     Inputs:
!       trS           - Trace of consistent part of matrix (Scaled)
!       ul(ndf,nen)   - Solution parameters
!       ndf           - Maximum number dof/node
!       nel           - Number of element nodes
!       nst           - Element matrix/vector dimension
!       nds           - No. dof's to scale (less than or = ndf)
!       isw           - Mode: 3 = stiffness; 5 = mass

!     Outputs:
!       s(nst,nst)    - Stabilization matrix (stiffness or mass)
!       r(ndf,*)      - Stabilization vector (residual)
!-----[--.----+----.----+----.-----------------------------------------]
      implicit none

      include  'qudshp.h'
      include  'vem_data.h'

      integer          :: ndf,nel,nst, nds, isw
      integer          :: i,j,k
      real    (kind=8) :: trS
      real    (kind=8) :: ul(ndf,*)
      real    (kind=8) :: r(ndf,*)
      real    (kind=8) :: s(nst,nst)

      save

!     Build the stabilization matrix

      dd(:,:) = 0.0d0
      do i = 1,nk
        do j = 1,nk
          do k = 1,nel
            dd(i,j) = dd(i,j) + Dmat(k,i)*Dmat(k,j)
          end do ! k
        end do ! j
      end do ! i

      call invert(dd,nk,10)

      dd(:,:) = dd(:,:)*trS

      do j = 1,nel
        do i = 1,nk
          dDmat(i) = 0.0d0
          do k = 1,nk
            dDmat(i) = dDmat(i) + dd(i,k)*Dmat(j,k)
          end do ! k
        end do ! i
        do i = 1,nel
          ss(i,j) = 0.0d0
          do k = 1,nk
            ss(i,j) = ss(i,j) - Dmat(i,k)*dDmat(k)
          end do ! k
        end do ! i
        ss(j,j) = ss(j,j) + trS
      end do ! j

!     Add stabilizing matrix to consistent term

      do j = 1,nel
        do i = 1,nel
          do k = 1,nds
            s(sa(i)+k,sa(j)+k) = s(sa(i)+k,sa(j)+k) + ss(i,j)
          end do ! k
        end do ! i
      end do ! j

!     Stabilizing residual

      if(isw.eq.3 .or. isw.eq.6) then
        do i = 1,nel
          do j = 1,nel
            do k = 1,nds
              r(k,i) = r(k,i) - ss(i,j)*ul(k,j)
            end do ! j
          end do ! j
        end do ! i
      elseif(isw.eq.5) then
        do i = 1,nel
          r(1,i) = 0.0d0
          do j = 1,nel
            r(1,i) = ss(i,j) + r(1,i)
          end do ! j
          do j = 2,nds
            r(j,i) = r(1,i)
          end do ! j
        end do ! i
      endif

      end subroutine vem_stab2d

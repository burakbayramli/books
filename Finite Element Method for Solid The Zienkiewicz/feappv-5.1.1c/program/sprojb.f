!$Id:$
      subroutine sprojb(b,v,t,h,neq,nv,imas)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute subspace projection of 'b' to form 'h'

!      Inputs:
!         b(*)     - Symmetric coefficient matrix for eigenproblem
!         v(neq,*) - Set of iteration vectors
!         neq      - Number of equations in B
!         nv       - Size of projected matrix
!         imas     - Mass type: 1 = consistent; 2 = diagonal.

!      Scratch:
!         t(neq)   - Working vector

!      Outputs:
!         h(*)     - Projected matrix V_trans * B * V
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'pointer.h'
      include  'comblk.h'

      integer   neq,nv,imas, i,j,k
      real (kind=8) :: b(*),v(neq,*),t(*),h(*), dot

      save

!     Compute 'z' and 'b' projection to form 'h'

      do j = 1,nv

!       Consistent mass

        if(imas.eq.1) then
          call pzero(t,neq)
          call caprod(b(1),b(neq+1),v(1,j),t,mr(np(90)),mr(np(91)),neq)

!       Lumped mass

        else
          do i = 1,neq
            t(i) = v(i,j)*b(i)
          end do
        endif

!       Project 'z' and 'v' vectors to form 'h'

        k = j*(j+1)/2
        do i = j,nv
          h(k) = dot(t,v(1,i),neq)
          k = k + i
        end do
        do i = 1,neq
          v(i,j) = t(i)
        end do
      end do

      end

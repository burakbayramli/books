!$Id:$
      subroutine sproja(v,t,g,neq,nv)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute subspace projection of 'a' to form 'g'

!      Inputs:
!         v(neq,*) - Set of iteration vectors
!         neq      - Number of equations in A
!         nv       - Size of projected matrix

!      Scratch:
!         t(neq)   - Working vector

!      Outputs:
!         g(*)     - Projected matrix V_trans * A * V
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'pointer.h'
      include  'comblk.h'

      integer   neq,nv, i,j,k
      real (kind=8) :: en

      real (kind=8) :: v(neq,*),t(neq),g(*)

      real (kind=8) :: dot

      save

!     Forward reduce eigenvector estimates

      k = 0
      do j = 1,nv

!     Copy vector 'v' into 'z' and solve equations

        call pmove (v(1,j),t(1),neq)
        call dasol (hr(np(1)+neq),hr(np(1)+neq),hr(np(1)),
     &              v(1,j),mr(np(21)),neq,neq,en)

!     Compute projection of stiffness

        do i = 1,j
          k = k + 1
          g(k) = dot(v(1,i),t(1),neq)
        end do
      end do

      end

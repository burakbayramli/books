c$Id:$
      subroutine sproja(v,t,g,neq,nv)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Compute subspace projection of 'a' to form 'g'

c      Inputs:
c         v(neq,*) - Set of iteration vectors
c         neq      - Number of equations in A
c         nv       - Size of projected matrix

c      Scratch:
c         t(neq)   - Working vector

c      Outputs:
c         g(*)     - Projected matrix V_trans * A * V
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'pointer.h'
      include  'comblk.h'

      integer   neq,nv, i,j,k
      real*8    en

      real*8    v(neq,*),t(neq),g(*)

      real*8    dot

      save

c     Forward reduce eigenvector estimates

      k = 0
      do j = 1,nv

c     Copy vector 'v' into 'z' and solve equations

        call pmove (v(1,j),t(1),neq)
        call dasol (hr(np(1)+neq),hr(np(1)+neq),hr(np(1)),
     &              v(1,j),mr(np(21)),neq,neq,en)

c     Compute projection of stiffness

        do i = 1,j
          k = k + 1
          g(k) = dot(v(1,i),t(1),neq)
        end do
      end do

      end

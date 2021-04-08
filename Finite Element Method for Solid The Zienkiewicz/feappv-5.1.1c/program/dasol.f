!$Id:$
      subroutine dasol(al,au,ad,b,jp,neqs, neqt, energy)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Solution of algebraic equations stored in profile form

!         Equations '   1  ' to 'neqs' are symmetric.
!         Equations 'neqs+1' to 'neqt' are unsymmetric.

!         Use:
!          a.) All equations are unsymmetric       : neqs = 1 (or 0)
!              N.B.  The top 1 x 1 submatrix is always symmetric.
!                    Both 'al' and 'au' must be provided.

!          b.) All equations are symmetric         : neqs = neqt
!              N.B.  In this case the array 'al' is not used.

!          c.) First 'neqs' equations are symmetric: 1 < neqs < neqt
!              N.B.  Storage of 'al' for unsymmetric equations only.

!      Coefficient matrix must be decomposed into its triangular
!      factors using 'DATRI' before using 'DASOL'.

!      Inputs:
!         al(*)  - Lower triangular factors of A
!         au(*)  - Upper triangular factors of A
!         ad(*)  - Diagonal factors of A
!         b(*)   - Right hand side vector
!         jp(*)  - Pointer array for row/columns of 'al', 'au'.
!         neqs   - Number of symmetric equations
!         neqt   - Number of equations to solve.

!      Outputs:
!         b(*)   - Solution vector, x.
!         energy - Energy of solution: x*A*x
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'
      include  'setups.h'

      integer       :: jp(*)
      integer       :: is, j, jr, jh, neqs, neqt, neq
      real (kind=8) :: bd, energy, al(*),au(*),ad(*),b(*), dot

      save

!     Initialize energy

      energy = 0.0d0

!     Find first non-zero entry in right hand side

      do is = 1,neqt
        if(b(is).ne.0.0d0) go to 100
      end do
      if(rank.eq.0) then
        if(ior.gt.0) write(iow,2000)
        if(ior.lt.0) write(*,2000)
       endif
      return

!     Reduce right hand side

!     Do symmetric part

100   neq = max(1, neqs)
      do j = is+1,neq
        jr = jp(j-1)
        jh = jp(j) - jr
        if(jh.gt.0) then
          b(j) = b(j) - dot(au(jr+1),b(j-jh),jh)
        endif
      end do

!     Do unsymmetric part

      do j = max(is,neq)+1,neqt
        jr = jp(j-1)
        jh = jp(j) - jr
        if(jh.gt.0) then
          jr   = jr   - jp(neq)
          b(j) = b(j) - dot(al(jr+1),b(j-jh),jh)
        endif
      end do

!     Multiply by inverse of diagonal elements

      do j = is,neqt
        bd = b(j)
        b(j) = b(j)*ad(j)
        energy = energy + bd*b(j)
      end do

!     Symmetric and unsymmetric backsubstitution

      do j = neqt,2,-1
        jr = jp(j-1)
        jh = jp(j) - jr
        if(jh.gt.0) then
          call colred(au(jr+1),b(j),jh, b(j-jh))
        endif
      end do

!     Warning format

2000  format(' *WARNING* Zero right-hand-side vector')

      end subroutine dasol

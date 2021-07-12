c$Id:$
      subroutine dasol(al,au,ad,b,jp,neqs, neqt, energy)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Solution of algebraic equations stored in profile form

c         Equations '   1  ' to 'neqs' are symmetric.
c         Equations 'neqs+1' to 'neqt' are unsymmetric.

c         Use:
c          a.) All equations are unsymmetric       : neqs = 1 (or 0)
c              N.B.  The top 1 x 1 submatrix is always symmetric.
c                    Both 'al' and 'au' must be provided.

c          b.) All equations are symmetric         : neqs = neqt
c              N.B.  In this case the array 'al' is not used.

c          c.) First 'neqs' equations are symmetric: 1 < neqs < neqt
c              N.B.  Storage of 'al' for unsymmetric equations only.

c      Coefficient matrix must be decomposed into its triangular
c      factors using 'DATRI' before using 'DASOL'.

c      Inputs:
c         al(*)  - Lower triangular factors of A
c         au(*)  - Upper triangular factors of A
c         ad(*)  - Diagonal factors of A
c         b(*)   - Right hand side vector
c         jp(*)  - Pointer array for row/columns of 'al', 'au'.
c         neqs   - Number of symmetric equations
c         neqt   - Number of equations to solve.

c      Outputs:
c         b(*)   - Solution vector, x.
c         energy - Energy of solution: x*A*x
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      integer   jp(*)
      integer   is, j, jr, jh, neqs, neqt, neq
      real*8    bd, energy, al(*),au(*),ad(*),b(*), dot

      save

c     Initialize energy

      energy = 0.0d0

c     Find first non-zero entry in right hand side

      do is = 1,neqt
        if(b(is).ne.0.0d0) go to 100
      end do
      if(ior.gt.0) write(iow,2000)
      if(ior.lt.0) write(*,2000)
      return

c     Reduce right hand side

c     Do symmetric part

100   neq = max(1, neqs)
      do j = is+1,neq
        jr = jp(j-1)
        jh = jp(j) - jr
        if(jh.gt.0) then
          b(j) = b(j) - dot(au(jr+1),b(j-jh),jh)
        endif
      end do

c     Do unsymmetric part

      do j = max(is,neq)+1,neqt
        jr = jp(j-1)
        jh = jp(j) - jr
        if(jh.gt.0) then
          jr   = jr   - jp(neq)
          b(j) = b(j) - dot(al(jr+1),b(j-jh),jh)
        endif
      end do

c     Multiply by inverse of diagonal elements

      do j = is,neqt
        bd = b(j)
        b(j) = b(j)*ad(j)
        energy = energy + bd*b(j)
      end do

c     Symmetric and unsymmetric backsubstitution

      do j = neqt,2,-1
        jr = jp(j-1)
        jh = jp(j) - jr
        if(jh.gt.0) then
          call colred(au(jr+1),b(j),jh, b(j-jh))
        endif
      end do

c     Warning format

2000  format(' *WARNING* Zero right-hand-side vector')

      end

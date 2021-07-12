c$Id:$
      subroutine caprod(ad,ac,p,v,jc,ir,neq)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose:  Peform a matrix vector product (A*p) for a symmetric
c                matrix stored in compact form (rows below diagonals,
c                columns above), diagonal stored separtely in 'ad'.

c      Inputs:
c         ad(*)  - Diagonal entries for A array
c         ac(*)  - Off-diagonal entries for symmetric matrix
c         p(*)   - Specified vector for product
c         jc(*)  - Pointer array to locate entries in rows/columns
c         ir(*)  - Location of non-zero entries in A
c         neq    - Number of equations

c      Outputs:
c         v(*)   - Matrix product of A*p.

c-----[--.----+----.----+----.-----------------------------------------]
      implicit none

      integer   neq,ni,nj,nj0,njd
      real*8    vni

      integer   jc(*),ir(*)
      real*8    ad(*),ac(*),p(*),v(*)

c     Diagonal part

      do ni = 1,neq
        v(ni) = ad(ni)*p(ni)
      end do

c     Loop over number of equations

      do ni = 2,neq

c     Perform multiplies for non-zero terms

        nj0 = jc(ni-1)
        njd = jc(ni) - nj0
        vni = 0.0d0

        do nj = 1,njd

c         Lower part

          vni           = vni + ac(nj+nj0)*p(ir(nj+nj0))

c         Upper part

          v(ir(nj+nj0)) = v(ir(nj+nj0)) + ac(nj+nj0)*p(ni)

        end do
        v(ni) = v(ni) + vni

      end do

      end

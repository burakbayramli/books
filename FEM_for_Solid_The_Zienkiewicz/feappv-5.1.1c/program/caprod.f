!$Id:$
      subroutine caprod(ad,ac,p,v,jc,ir,neq)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Peform a matrix vector product (A*p) for a symmetric
!                matrix stored in compact form (rows below diagonals,
!                columns above), diagonal stored separtely in 'ad'.

!      Inputs:
!         ad(*)  - Diagonal entries for A array
!         ac(*)  - Off-diagonal entries for symmetric matrix
!         p(*)   - Specified vector for product
!         jc(*)  - Pointer array to locate entries in rows/columns
!         ir(*)  - Location of non-zero entries in A
!         neq    - Number of equations

!      Outputs:
!         v(*)   - Matrix product of A*p.

!-----[--.----+----.----+----.-----------------------------------------]
      implicit none

      integer       :: neq,ni,nj,nj0,njd
      real (kind=8) :: vni

      integer       :: jc(*),ir(*)
      real (kind=8) :: ad(*),ac(*),p(*),v(*)

!     Diagonal part

      do ni = 1,neq
        v(ni) = ad(ni)*p(ni)
      end do

!     Loop over number of equations

      do ni = 2,neq

!     Perform multiplies for non-zero terms

        nj0 = jc(ni-1)
        njd = jc(ni) - nj0
        vni = 0.0d0

        do nj = 1,njd

!         Lower part

          vni           = vni + ac(nj+nj0)*p(ir(nj+nj0))

!         Upper part

          v(ir(nj+nj0)) = v(ir(nj+nj0)) + ac(nj+nj0)*p(ni)

        end do
        v(ni) = v(ni) + vni

      end do

      end subroutine caprod

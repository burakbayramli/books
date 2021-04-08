!$Id:$
      subroutine numass(b,neq,mq)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Determine number of diagonal entries in B array of
!               generalized eigen problem:  A*x = B*x*lambda

!      Inputs:
!         b(*)  - Diagonals of B array
!         neq   - Number of equations

!      Outputs:
!         mq    - Number of non-zero entries in B-diagonal
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      integer       :: n,nn,neq,mq
      real (kind=8) :: b(*)

      nn = 0
      do n = 1,neq
        if(b(n).ne.0.0d0) nn = nn + 1
      end do

      if(nn.lt.mq) then
        write(iow,2000) nn
        if(ior.lt.0) then
          write(*,2000) nn
        endif
      endif

      mq = min0(mq,nn)

!     Format

2000  format(' Subspace reduced to',i4,' by number of nonzero',
     &       ' diagonal mass terms')

      end subroutine numass

c$Id:$
      subroutine numass(b,neq,mq)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Determine number of diagonal entries in B array of
c               generalized eigen problem:  A*x = B*x*lambda

c      Inputs:
c         b(*)  - Diagonals of B array
c         neq   - Number of equations

c      Outputs:
c         mq    - Number of non-zero entries in B-diagonal
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      integer   n,nn,neq,mq
      real*8    b(*)

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

c     Format

2000  format(' Subspace reduced to',i4,' by number of nonzero',
     &       ' diagonal mass terms')

      end

c$Id:$
      subroutine chlbac(u,s,nn)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Back substitution for Cholesky factors in eigen
c               solutions.

c      Inputs:
c        u(*)   - Unreduced array
c        s(*,*) - Factored array of matrix
c        nn     - Size of arrays

c      Outputs:
c        u(*)   - Solution after back substitution

c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer   i,j,jd, nn
      real*8    u(*),s(nn,nn)

      save

c     Compute eigenvalues of general linear problem by backsubstitution

      j  = nn
      jd = nn*(nn+1)/2
      do i = 1,nn
        s(nn,i) = s(nn,i)*u(jd)
      end do

      do j = nn,2,-1
        jd = jd - j
        do i = 1,nn
          call colbac(u(jd+1),s(1,i),u(jd),j-1)
        end do
      end do

      end

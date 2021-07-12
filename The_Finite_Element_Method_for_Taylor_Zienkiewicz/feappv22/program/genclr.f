c$Id:$
      subroutine genclr(ndf, v, nty, numnp)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Remove initial conditions on nodes merged by tie command

c      Inputs:
c         ndf      - Number dof/node
c         nty(*)   - Nodal type
c         numnp    - Number of nodes

c      Outputs:
c         v(ndf,*) - Initial conditions with merged nodes removed
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer   ndf,numnp, i,n, nty(numnp)
      real*8    v(ndf,numnp)

      do n = 1,numnp
        if(nty(n) .lt. 0) then
          do i = 1,ndf
            v(i,n) = 0.0d0
          end do
        endif
      end do

      end

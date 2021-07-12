c$Id:$
      subroutine maxcor(x,ndm,numnp)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Determine max/min coordinates for a perspective
c               view selection

c      Inputs:
c         x(ndm,*)  - Nodal coordinates (may be in deformed state)
c         ndm       - Spatial dimension
c         numnp     - Number of nodes in mesh

c      Outputs:
c         none      - Output through common /pdata0/
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'pdata0.h'
      include  'pdatas.h'
      include  'pointer.h'
      include  'comblk.h'

      integer   ndm, numnp, i,ii,n
      integer   nty
      real*8    x(ndm,*)

      save

c     Determine max/min coordinates for a perspective view selection

      ii  = min(ndm,3)
      nty = np(49) - 1
      do i = 1,ii
        vmin(i) = x(i,1)
        do n = 1,numnp
            vmin(i) = max(vmin(i),x(i,n))
        end do
        vmax(i) = vmin(i)
        do n = 1,numnp
          if(mr(nty+n).ge.0) then
            vmin(i) = min(vmin(i),x(i,n))
          endif
        end do
      end do

      end

c$Id:$
      function pdiff(x,i,ndm,numnp)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Compute the difference between maximum and minimum
c               nodal coordinates in direction-i.

c      Inputs:
c         x(ndm,* ) - Nodal coordinates for mesh
c         i         - Direction of comparison
c         ndm       - Spatial dimension of mesh
c         numnp     - Number of nodes in mesh

c      Outputs:
c         pdiff     - Difference between maximum and minimum
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'iofile.h'
      include  'pointer.h'
      include  'comblk.h'

      integer   i, n, ndm, numnp, nty
      real*8    pdiff,  xmx, xmn, x(ndm,numnp)

      save

      nty = np(49) - 1
      do n = 1,numnp
         if(mr(nty+n).ge.0) go to 110
      end do

      if(ior.gt.0) write(iow,2000)
      if(ior.lt.0) write(*,2000)
      pdiff = 0.0
      return

110   xmx = x(i,n)
      xmn = x(i,n)
      do n = 1,numnp
         if(mr(nty+n).ge.0) then
            xmx = max(xmx,x(i,n))
            xmn = min(xmn,x(i,n))
         endif
      end do

      pdiff = xmx - xmn

c     Format

2000  format(' *ERROR* Coodinates are unspecified')

      end

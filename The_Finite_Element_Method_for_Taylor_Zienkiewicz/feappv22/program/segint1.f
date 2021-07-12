c$Id:$
      subroutine segint1(xs,side,is,ns,ndm,shp, x)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c     Purpose: Construct one dimensional segment interpolation for coords.

c     Inputs:
c       xs(3,*)   - Nodal values for interpolation function
c       side      - side number (check sign)
c       is(*)     - List of side nodes and increments
c       ns        - Order of Lagrange polynomial for side
c       ndm       - Spatial dimension of mesh
c       shp(*)    - Shape functions for nodal values

c     Outputs:
c       x(ndm,ip) - Coordinates of points
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer   ns,ndm, side, i, j, ib, ie, in, io, i1, isr(2), is(*)
      real*8    xs(3,*), shp(*), x(ndm,*), xsn(3,2)

      save

      data      isr / 1,2 /

c     Check sign
      if(side.gt.0) then
        ib =  1
        ie =  ns - 1
        in =  2
        io =  1
      else
        ib =  ns
        ie =  2
        in = -2
        io = -1
      endif

c     Compute segment values

      i1 = 1
      do i = ib,ie,in
        do j = 1,ndm
          xsn(j,1) = xs(j,is(i   ))
          xsn(j,2) = xs(j,is(i+in))
        end do ! j

c       Interpolate individual segments

        call interp1(is(i+io),xsn, 1,isr,2,ndm,shp, x(1,i1))

        i1 = i1 + is(i+io)

      end do ! i

      end

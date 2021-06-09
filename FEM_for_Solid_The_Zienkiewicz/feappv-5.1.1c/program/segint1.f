!$Id:$
      subroutine segint1(xs,side,is,ns,ndm,shp, x)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Construct one dimensional segment interpolation for coords.

!     Inputs:
!       xs(3,*)   - Nodal values for interpolation function
!       side      - side number (check sign)
!       is(*)     - List of side nodes and increments
!       ns        - Order of Lagrange polynomial for side
!       ndm       - Spatial dimension of mesh
!       shp(*)    - Shape functions for nodal values

!     Outputs:
!       x(ndm,ip) - Coordinates of points
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: ns,ndm, side, i,j, ib,ie,in,io,i1, isr(2),is(*)
      real (kind=8) :: xs(3,*), shp(*), x(ndm,*), xsn(3,2)

      save

      data      isr / 1,2 /

!     Check sign
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

!     Compute segment values

      i1 = 1
      do i = ib,ie,in
        do j = 1,ndm
          xsn(j,1) = xs(j,is(i   ))
          xsn(j,2) = xs(j,is(i+in))
        end do ! j

!       Interpolate individual segments

        call interp1(is(i+io),xsn, 1,isr,2,ndm,shp, x(1,i1))

        i1 = i1 + is(i+io)

      end do ! i

      end subroutine segint1

!$Id:$
      subroutine interp1(nr,xs,side,is,ns,ndm,shp, x)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Construct one dimensional Lagrange interpolation for coords.

!     Inputs:
!       nr        - Number of increments on side
!       xs(3,*)   - Nodal values of interpolation function
!       side      - side number (check sign)
!       is(*)     - List of side nodes
!       ns        - Order of Lagrange polynomial for side
!       ndm       - Spatial dimension of mesh
!       shp(*)    - Shape functions for nodal values

!     Outputs:
!       x(ndm,ip) - Coordinates of points
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer       :: i,i1, j, m,m1,m2, n1,n2,n3,nr,ns,ndm, side,is(*)
      real (kind=8) :: den, rn,xi, xid,xii,xij, xs(3,*),shp(*),x(ndm,*)

      save

!     Set order of using nodes on side

      if(side.gt.0) then
        m1 = 1
        m2 = 2
        n1 = 3
        n2 = ns
        n3 = 1
      elseif(side.lt.0) then
        m1 = 2
        m2 = 1
        n1 = ns
        n2 = 3
        n3 = -1
      endif

!     Left and right ends

      do j = 1,ndm
        x(j,   1) = xs(j,is(m1))
        x(j,nr+1) = xs(j,is(m2))
      end do ! j

      xid = 1.0d0/dble(nr)
      rn  = 1.0d0/dble(ns-1)

!     Loop through interior points

      do m = 2,nr

        xi = dble(m-1)*xid

!       Compute Lagrange interpolation function at point 'xi'

!       End nodes

        shp(1) = 1.0d0
        den    = 1.0d0
        xii    = 0.0d0
        do j = 2,ns
          xij    = dble(j-1)*rn
          shp(1) = shp(1)*(xi - xij)
          den    = den * (xii - xij)
        end do ! j
        shp(1) = shp(1)/den

        shp(2) = 1.0d0
        den    = 1.0d0
        xii    = 1.0d0
        do j = 1,ns-1
          xij    = dble(j-1)*rn
          shp(2) = shp(2)*(xi - xij)
          den    = den * (xii - xij)
        end do ! j
        shp(2) = shp(2)/den

        do i = 3,ns

          shp(i) = 1.0d0
          den    = 1.0d0
          xii    = dble(i-2)*rn
          do j = 1,ns
            if(i-1.ne.j) then
              xij    = dble(j-1)*rn
              shp(i) = shp(i)*(xi - xij)
              den    = den * (xii - xij)
            end if
          end do ! j
          shp(i) = shp(i)/den

        end do ! i

!       Perform interpolation for current shape function

        do j = 1,ndm
          x(j,m) = shp(1)*xs(j,is(m1)) + shp(2)*xs(j,is(m2))
        end do ! j

        i1 = 2
        do i = n1,n2,n3
          i1 = i1 + 1
          do j = 1,ndm
            x(j,m) = x(j,m) + shp(i1)*xs(j,is(i))
          end do ! j
        end do ! i
      end do ! m

      end subroutine interp1

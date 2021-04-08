!$Id:$
      subroutine arcint1(nr,xs,side,is,ns,ndm,shp, x, rs)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Construct one dimensional arc interpolation for coords.

!     Inputs:
!       nr        - Number of increments on side
!       xs(3,*)   - Nodal values of interpolation function
!       side      - side number (check sign)
!       is(*)     - List of side nodes
!       ns        - Order of Lagrange polynomial for side
!       ndm       - Spatial dimension of mesh
!       shp(*)    - Shape functions for nodal values

!     Scratch
!       rs(3,*)   - Radii and angles

!     Outputs:
!       x(ndm,*)  - Coordinates of points
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: nr,ns,ndm, side, n, j
      real (kind=8) :: ang

      integer       :: is(*), isr(10)
      real (kind=8) :: xs(3,*), shp(*), x(ndm,*), rs(3,*), dx(3)
      real (kind=8) :: e(3,3), elen

      save

!     For 3-d Form transformation to local frame

      if(ndm.eq.3) then
        do j = 1,ndm
          e(1,j) = xs(j,is(1)) - xs(j,is(ns))
          e(2,j) = xs(j,is(2)) - xs(j,is(ns))
        end do ! j
        e(3,1) = e(1,2)*e(2,3) - e(1,3)*e(2,2)
        e(3,2) = e(1,3)*e(2,1) - e(1,1)*e(2,3)
        e(3,3) = e(1,1)*e(2,2) - e(1,2)*e(2,1)
        e(2,1) = e(3,2)*e(1,3) - e(3,3)*e(1,2)
        e(2,2) = e(3,3)*e(1,1) - e(3,1)*e(1,3)
        e(2,3) = e(3,1)*e(1,2) - e(3,2)*e(1,1)
        do j = 1,ndm
          elen = 1.d0/sqrt(e(j,1)**2 + e(j,2)**2 + e(j,3)**2)
          do n = 1,3
            e(j,n) = e(j,n)*elen
          end do ! n
        end do ! j
      end if

!     Compute cylindrical frame

      do n = 1,ns-1
        do j = 1,ndm
          dx(j) = xs(j,is(n)) - xs(j,is(ns))
        end do
        if(ndm.eq.2) then
          rs(1,n) = sqrt(dx(1)**2 + dx(2)**2)
          rs(2,n) = atan2(dx(2),dx(1))
        elseif(ndm.eq.3) then
          do j = 1,ndm
            rs(j,n) = e(j,1)*dx(1) + e(j,2)*dx(2) + e(j,3)*dx(3)
          end do ! j
          dx(1)   = rs(1,n)
          rs(1,n) = sqrt(rs(1,n)**2 + rs(2,n)**2)
          rs(2,n) = atan2(rs(2,n),dx(1))
        endif
        isr(n)  = n
      end do

!     Check that second angle is larger than first

      if(rs(2,2) .le. rs(2,1)) then

!       Add 2*pi to each angle less than previous one

        ang = 8.d0*atan(1.d0)

        do n = 2,ns-1
          if(rs(2,n).le.rs(2,1)) then
            rs(2,n) = rs(2,n) + ang
          end if
        end do ! n

      end if

!     Interpolate the radii and angles

      call interp1(nr,rs,side,isr,ns-1,ndm,shp, x)

!     Position end points

      do j = 1,ndm

        if(side.gt.0) then
          x(j,1   ) = xs(j,is(1))
          x(j,nr+1) = xs(j,is(2))
        else
          x(j,nr+1) = xs(j,is(1))
          x(j,1   ) = xs(j,is(2))
        endif

      end do ! j

!     Convert r-theta coordinates to x-y values

      if(ndm.eq.2) then
        do n = 2,nr
          dx(1)  = x(1,n)*cos(x(2,n))
          dx(2)  = x(1,n)*sin(x(2,n))
          x(1,n) = dx(1)  + xs(1,is(ns))
          x(2,n) = dx(2)  + xs(2,is(ns))
        end do ! n

!     Transform 3-d case

      elseif(ndm.eq.3) then
        do n = 2,nr
          dx(1)  = x(1,n)*cos(x(2,n))
          dx(2)  = x(1,n)*sin(x(2,n))
          x(1,n) = dx(1)
          x(2,n) = dx(2)
          do j = 1,ndm
            dx(j) = e(1,j)*x(1,n) + e(2,j)*x(2,n) + e(3,j)*x(3,n)
          end do ! j
          do j = 1,ndm
            x(j,n) = dx(j)  + xs(j,is(ns))
          end do ! j
        end do ! n
      endif

      end subroutine arcint1

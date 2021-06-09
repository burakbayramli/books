!$Id:$
      subroutine pside1(nr,xs,tr,side,is,ns,ndm,shp,rt, x, styp)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]

!     Purpose: Construct one dimensional side interpolation for coords.

!     Inputs:
!       nr        - Number of increments on side
!       xs(3,*)   - Nodal values of interpolation function
!       side      - side number (check sign)
!       is(*)     - List of side nodes
!       ns        - Order of Lagrange polynomial for side
!       ndm       - Spatial dimension of mesh
!       shp(*)    - Shape functions for nodal values
!       rt(3,*)   - Temporary storage for polar coordinates
!       styp      - Type of edge: 0 = Lagrange interpolation
!                                 1 = Radius/angle interpolation

!     Outputs:
!       x(ndm,ip) - Coordinates of points

!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'pointer.h'
      include  'comblk.h'

      integer       :: k,n, nr,ns,side,ndm, styp

      integer       :: is(*)
      real (kind=8) :: xs(3,*),shp(*),rt(3,*),x(ndm,*),xx(3),tr(3,4)

      save

!     Lagrange interpolation

      if(styp.eq.0) then

        call interp1(nr,xs,side,is,ns,ndm,shp, x)

!     R/theta interpolation

      elseif(styp.eq.1) then

        call arcint1(nr,xs,side,is,ns,ndm,shp, x,rt)

!     R/theta interpolation

      elseif(styp.eq.2) then

        call segint1(xs,side,is,ns,ndm,shp, x)

!     Eliptical interpolation

      elseif(styp.eq.3) then

        call elpint1(nr,xs,side,is,ns,ndm,shp, x,rt)

      endif

!     Transform to current frame

      do n = 1,nr+1
        do k = 1,ndm
          xx(k) = x(k,n)
        end do ! k
        do k = 1,ndm
          x(k,n) = tr(k,4)+tr(k,1)*xx(1)+tr(k,2)*xx(2)+tr(k,3)*xx(3)
        end do ! k
      end do ! n

      end subroutine pside1

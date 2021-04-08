!$Id:$
      logical function zoom(xl,ndm)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Determine if point is in plot region

!      Inputs:
!         xl(ndm) - Nodal coordinates for point
!         ndm     - Dimension of xl array

!      Outputs:
!         zoom    - True if point in region
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'pdata1.h'
      include  'pdata4.h'
      include  'ppers.h'
      include  'pointer.h'
      include  'comblk.h'

      logical       :: errv
      integer       :: ndm, ntyp(1)
      real (kind=8) :: xm, ym, xx, yx, zm, zx, xc,yc, ddx,ddy

      real (kind=8) :: xl(*),xg(3)

      save

      data      ntyp / 1 /

!     Compute test for point in window
      if (fact .eq. 0.0d0) fact = 1.0d0
      xc = 0.5d0*(xmax(1) + xmin(1))
      yc = 0.5d0*(xmax(2) + xmin(2))
      ddx = 0.5d0*(xmax(1) - xmin(1))/fact
      ddy = 0.5d0*(xmax(2) - xmin(2))/fact
      xm = xl(1)
      ym = xl(2)
      if(kpers.ne.1) then
        xm = xm - (0.5d0 - s0(1))/(scalef*2.d0)
        ym = ym - (0.5d0 - s0(2))/(scalef*2.d0)
      endif
      xx = xm
      yx = ym
      zm = 0.d0
      if(ndm.ge.3) zm = xl(3)
      zx = zm

      if(kpers.eq.1) then
        xg(1) = xm
        xg(2) = ym
        xg(3) = zm
        call perspj(xg,ntyp,1,errv)
        xm    = xg(1)
        ym    = xg(2)
        xg(1) = xx
        xg(2) = yx
        xg(3) = zx
        call perspj(xg,ntyp,1,errv)
        xx    = xg(1)
        yx    = xg(2)
      endif

      zoom = ( (xm.ge.xc-ddx) .and. (xx.le.xc+ddx)
     &   .and. (ym.ge.yc-ddy) .and. (yx.le.yc+ddy) )

      end function zoom

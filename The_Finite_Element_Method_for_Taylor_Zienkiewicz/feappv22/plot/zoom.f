c$Id:$
      logical function zoom(xl,ndm)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Determine if point is in plot region

c      Inputs:
c         xl(ndm) - Nodal coordinates for point
c         ndm     - Dimension of xl array

c      Outputs:
c         zoom    - True if point in region
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'pdata1.h'
      include  'pdata4.h'
      include  'ppers.h'
      include  'pointer.h'
      include  'comblk.h'

      logical   errv
      integer   ndm
      real*8    xm, ym, xx, yx, zm, zx, xc,yc, ddx,ddy

      real*8    xl(*),xg(3)

      save

c     Compute test for point in window

      if (fact .eq. 0.0d0) fact = 1.0d0
      xc = 0.5d0*(xmax(1) + xmin(1))
      yc = 0.5d0*(xmax(2) + xmin(2))
      ddx = 0.5d0*(xmax(1) - xmin(1))/fact
      ddy = 0.5d0*(xmax(2) - xmin(2))/fact
      xm = xl(1)
      ym = xl(2)
      if(kpers.ne.1) then
        xm = xm - (0.5d0 - s0(1))/(scale*2.d0)
        ym = ym - (0.5d0 - s0(2))/(scale*2.d0)
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
        call perspj(xg,1,1,errv)
        xm    = xg(1)
        ym    = xg(2)
        xg(1) = xx
        xg(2) = yx
        xg(3) = zx
        call perspj(xg,1,1,errv)
        xx    = xg(1)
        yx    = xg(2)
      endif

      zoom = ( (xm.ge.xc-ddx) .and. (xx.le.xc+ddx)
     &   .and. (ym.ge.yc-ddy) .and. (yx.le.yc+ddy) )

      end

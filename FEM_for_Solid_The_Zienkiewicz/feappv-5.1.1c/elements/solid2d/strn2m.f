!$Id:$
      subroutine strn2m(d,xr1,shp,xl,ul,tl,stype,xr0,xz0,ndm,ndf,
     &                  nel,nen,ta,ep)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:

!      Inputs:

!      Outputs:
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'elm2d.h'

      integer       :: ndm,ndf,nel,nen,k
      real (kind=8) :: stype,xr1,xr0,xz0,ur,ta, theta

      real (kind=8) :: d(*),shp(3,*),xl(ndm,*),ul(ndf,nen),tl(*),ep(4)

      save

!     Compute strain tensor for constitutive equations

      ur    = 0.0d0
      xr0   = 0.0d0
      xz0   = 0.0d0
      ta    = -d(9)
      ep(1) = 0.0d0
      ep(2) = 0.0d0
      ep(4) = 0.0d0
      do k = 1,nel
        xr0   = xr0   + shp(3,k)*xl(1,k)
        xz0   = xz0   + shp(3,k)*xl(2,k)
        ta    = ta    + shp(3,k)*tl(k)
        ur    = ur    + shp(3,k)*ul(1,k)
        ep(1) = ep(1) + shp(1,k)*ul(1,k)
        ep(2) = ep(2) + shp(2,k)*ul(2,k)
        ep(4) = ep(4) + shp(1,k)*ul(2,k)
     &                + shp(2,k)*ul(1,k)
      end do
      xr1   = 1.0 + stype*(xr0 - 1.0)
      ep(3) = stype*ur/xr1

!     Correct strains and incremental strains for mixed formulation

      theta = 0.333333333333333d0*(trep - ep(1) - ep(2) - ep(3))
      ep(1) = ep(1) + theta
      ep(2) = ep(2) + theta
      ep(3) = ep(3) + theta

      end subroutine strn2m

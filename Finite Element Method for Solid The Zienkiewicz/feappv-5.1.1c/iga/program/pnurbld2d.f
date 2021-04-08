!$Id:$
      subroutine pnurbld2d(shp,xx,dxdxi,npres,nel,ndf, fl, de)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  2-d  user loads for NURBS

!      Inputs:
!        shp(4,nel)  - Shape functions and natural derivatives
!                      1 = derv 1; 2 = derv 2; 3 = derv 3; 4 = N_a
!        xx(ndm)     - Coordinates at quadrature point
!        dxdxi(3,3)  - Natural derivatives * jacobian * quad wt.
!        npres(3)    - Load parameters 1 and 2
!        nel         - Number of control points on boundary
!        ndf         - Dof's per control point

!      Outputs:
!        fl(ndf,nel) - Loads for control points
!        de          - ur*sigrr + ut*sigrt
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'iofile.h'

      logical    psflag, enerfl
      integer    nel,ndf, j

      real*8     de
      real*8     shp(4,nel),xx(2),dxdxi(3,3), npres(3), fl(ndf,nel)

      real*8     rr2, theta, tx,rad2,rad4, sigrr,sigtt,sigrt, ur,ut
      real*8     sigxx,sigyy,sigxy
      real*8     sn,cs, c2,s2,sc, nx,ny, chim,chip, mu, e, nu, bigr, rr
      real*8     sn2, cs2
      real*8     as,ab, bs,bb, ds,db, exact

!     Set computation of energy for circular region

      enerfl = .false.

!     Set flag true for plane strain

      psflag = .true.

!     Compute contribution to Neumann condiaion at pt xx(1-2).

      e    = 1.0d5
      nu   = 0.3d0
      mu   = e/(2.0d0*(1.d0 + nu))

!     Plane strain

      if(psflag) then

        chim = 2.0d0 - 4.0d0*nu         ! chi - 1
        chip = 4.0d0 - 4.0d0*nu         ! chi + 1

!     Plane stress

      else

        chim = 2.0d0*(1.0d0 - nu)/(1.0d0 + nu)
        chip = 4.0d0/(1.0d0 + nu)

      endif

!     Example: Tension loading on an infinite strip

      rr2   = xx(1)**2 + xx(2)**2
      theta = atan2(xx(2),xx(1))
      tx    = npres(1) * 0.5d0
      rad2  = npres(2)*npres(2)/rr2
      rad4  = rad2*rad2
      cs2   = cos(2.d0*theta)
      sn2   = sin(2.d0*theta)

      nx    = dxdxi(1,2) ! Includes jacobian
      ny    = dxdxi(2,2)

!     Compute polar components

      sigrr =  tx * (1.0d0 - rad2)
     &      +  tx * (1.0d0 - 4.0d0*rad2 + 3.0d0*rad4)*cs2
      sigtt =  tx * (1.0d0 + rad2)
     &      -  tx * (1.0d0 + 3.0d0*rad4)*cs2
      sigrt = -tx * (1.0d0 + 2.0d0*rad2 - 3.0d0*rad4)*sn2

!     Transform to cartesian components

      cs    =  cos(theta)
      sn    =  sin(theta)
      c2    =  cs*cs
      s2    =  sn*sn
      sc    =  sn*cs
!     sc    = -sn*cs

      sigxx = sigrr*c2 + sigtt*s2 - sigrt*sc*2.0d0
      sigyy = sigrr*s2 + sigtt*c2 + sigrt*sc*2.0d0
      sigxy = (sigrr - sigtt)*sc + sigrt*(c2-s2)

      do j = 1,nel
        fl(1,j) = fl(1,j) + (sigxx*nx + sigxy*ny)*shp(4,j)
        fl(2,j) = fl(2,j) + (sigxy*nx + sigyy*ny)*shp(4,j)
      end do ! j

!     Displacements

      if(enerfl) then
        bigr = npres(2)
        rr   = sqrt(rr2)

        ur = tx/(4.d0*mu*rr)*(chim * rr2 + 2.d0*bigr**2
     &     + 2.0d0*(chip * bigr**2 + rr2  - bigr**4/rr2)*cs2)

        ut = - tx/(2.d0*mu*rr)*(chim * bigr**2 + rr2 + bigr**4/rr2)*sn2

        de = ur*sigrr + ut*sigrt

!       Compute exact energy done

        as =  tx * (1.0d0 - rad2)
        bs =  tx * (1.0d0 - 4.0d0*rad2 + 3.0d0*rad4)
        ds = -tx * (1.0d0 + 2.0d0*rad2 - 3.0d0*rad4)

        ab =  tx/(4.d0*mu*rr)*(chim * rr2 + 2.d0*bigr**2)
        bb =  tx/(2.d0*mu*rr)*(chip * bigr**2 + rr2  - bigr**4/rr2)
        db = - tx/(2.d0*mu*rr) * (chim * bigr**2  + rr2 + bigr**4/rr2)

        exact = 1.5d0*acos(-1.0d0)*(as*ab + 0.5d0*(bs*bb + ds*db))

        write(iow,*) ' EXACT = ',exact

        cs2 = 1.0d0
        sn2 = 1.0d0
        rr  = 1.0d0
        rr2 = 1.0d0
        ur = tx/(4.d0*mu*rr)*(chim * rr2 + 2.d0*bigr**2
     &     + 2.0d0*(chip * bigr**2 + rr2  - bigr**4/rr2)*cs2)

        ut = tx/(4.d0*mu*rr)*(chim * rr2 + 2.d0*bigr**2)

        write(iow,*) ' u_x =',ur,' u_y =',ut

      endif

      end subroutine pnurbld2d

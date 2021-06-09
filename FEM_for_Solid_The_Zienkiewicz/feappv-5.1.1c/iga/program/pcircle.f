!$Id:$
      subroutine pcircle(xx,ndisp, ux,uy, psflag)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  2-d  user displacements for circlular hole NURBS

!      Inputs:
!        xx(ndm)     - Coordinates at quadrature point
!        ndisp(3)    - Load parameters 1 and 2

!      Outputs:
!        ux,uy       - x & y displacements
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      real*8     xx(2), ndisp(3)

      logical    psflag
      real*8     rr2, theta, tx, ur,ut, ux,uy
      real*8     sn,cs, chim,chip, mu, e, nu, bigr, rr
      real*8     sn2, cs2

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

!     Tension loading on an infinite strip

      rr2   = xx(1)**2 + xx(2)**2
      theta = atan2(xx(2),xx(1))
      tx    = ndisp(1) * 0.5d0
      cs2   = cos(2.d0*theta)
      sn2   = sin(2.d0*theta)

!     Compute polar components

      bigr = ndisp(2)
      rr   = sqrt(rr2)

      ur = tx/(4.d0*mu*rr)*(chim * rr2 + 2.d0*bigr**2
     &   + 2.0d0*(chip * bigr**2 + rr2  - bigr**4/rr2)*cs2)

      ut = - tx/(2.d0*mu*rr) * (chim * bigr**2  + rr2 + bigr**4/rr2)*sn2

!     Transform to cartesian components

      cs    =  cos(theta)
      sn    =  sin(theta)

      ux = (ur*cs - ut*sn)
      uy = (ur*sn + ut*cs)

      end

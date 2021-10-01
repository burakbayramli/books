!> @file timeStep.f90
!!
!! Computation of spectral radii and the time step.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: March 22, 2014
!
! *****************************************************************************
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
!
! *****************************************************************************

!> Computes spectral radii, maximum stable time step as well as coefficients
!! for the central implicit residual smoothing (CIRS).
!!
subroutine TimeStep

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  use ModInterfaces, only : BcondCutSingle, CompTheta
  implicit none

! local variables
  integer     :: itype, lb, lbeg, lend, lbs, lbegs, lends
  integer     :: i, j, iseg
  real(rtype) :: rrho, u, v, sx, sy, vc, cs, cflrat, reval, reval1, &
                 ex, ey, tsmin, fmue, f1, f2, fac, dtv, ds2, &
                 srvi, srvj, cfac
  real(rtype) :: rhop, rhoT, hT, q2, theta, a1, ra1g, a4, a5, ds

! *****************************************************************************
! Euler equations -------------------------------------------------------------

  if (kequs == "E") then

! - preconditioned equations

    if (kprecond == "Y") then
      do j=2,j2
        do i=2,i2
          rrho  = 1.D0/cv(1,i,j)
          u     = cv(2,i,j)*rrho
          v     = cv(3,i,j)*rrho
          rhop  = cv(1,i,j)/dv(1,i,j)
          rhoT  = -cv(1,i,j)/dv(2,i,j)
          hT    = dv(5,i,j)
          q2    = u*u + v*v
          theta = CompTheta( dv(4,i,j),dv(3,i,j),q2 )
          a1    = cv(1,i,j)*rhop*hT + rhoT
          ra1g  = 1.D0/(cv(1,i,j)*theta*hT+rhoT)
          a4    = a1*ra1g
          a5    = cv(1,i,j)*hT*ra1g

          sx       = 0.5D0*(si(1,i,j)+si(1,i+1,j))
          sy       = 0.5D0*(si(2,i,j)+si(2,i+1,j))
          ds       = Sqrt(sx*sx+sy*sy)
          sx       = sx/ds
          sy       = sy/ds
          vc       = sx*u + sy*v
          cs       = Sqrt((vc*vc)*((a4-1.D0)*(a4-1.D0))+4.D0*a5)
          sri(i,j) = 0.5D0*((a4+1.D0)*Abs(vc)+cs)*ds

          sx       = 0.5D0*(sj(1,i,j)+sj(1,i,j+1))
          sy       = 0.5D0*(sj(2,i,j)+sj(2,i,j+1))
          ds       = Sqrt(sx*sx+sy*sy)
          sx       = sx/ds
          sy       = sy/ds
          vc       = sx*u + sy*v
          cs       = Sqrt((vc*vc)*((a4-1.D0)*(a4-1.D0))+4.D0*a5)
          srj(i,j) = 0.5D0*((a4+1.D0)*Abs(vc)+cs)*ds

          tstep(i,j) = vol(i,j)/(sri(i,j)+srj(i,j))
        enddo
      enddo

! - original equations

    else
      do j=2,j2
        do i=2,i2
          rrho = 1.D0/cv(1,i,j)
          u    = cv(2,i,j)*rrho
          v    = cv(3,i,j)*rrho

          sx       = 0.5D0*(si(1,i,j)+si(1,i+1,j))
          sy       = 0.5D0*(si(2,i,j)+si(2,i+1,j))
          vc       = sx*u + sy*v
          cs       = dv(3,i,j)*Sqrt(sx*sx+sy*sy)
          sri(i,j) = Abs(vc) + cs

          sx       = 0.5D0*(sj(1,i,j)+sj(1,i,j+1))
          sy       = 0.5D0*(sj(2,i,j)+sj(2,i,j+1))
          vc       = sx*u + sy*v
          cs       = dv(3,i,j)*Sqrt(sx*sx+sy*sy)
          srj(i,j) = Abs(vc) + cs

          tstep(i,j) = vol(i,j)/(sri(i,j)+srj(i,j))
        enddo
      enddo
    endif ! kprecond

! Navier-Stokes equations (laminar) -------------------------------------------

  else

    if (kdissip == "C") then
      cfac = 4.D0
    else
      if (iorder > 1) then
        cfac = 1.D0
      else
        cfac = 2.D0
      endif
    endif

! - preconditioned equations

    if (kprecond == "Y") then
      do j=2,j2
        do i=2,i2
          rrho  = 1.D0/cv(1,i,j)
          u     = cv(2,i,j)*rrho
          v     = cv(3,i,j)*rrho
          rhop  = cv(1,i,j)/dv(1,i,j)
          rhoT  = -cv(1,i,j)/dv(2,i,j)
          hT    = dv(5,i,j)
          q2    = u*u + v*v
          theta = CompTheta( dv(4,i,j),dv(3,i,j),q2 )
          a1    = cv(1,i,j)*rhop*hT + rhoT
          ra1g  = 1.D0/(cv(1,i,j)*theta*hT+rhoT)
          a4    = a1*ra1g
          a5    = cv(1,i,j)*hT*ra1g

          fmue = dv(6,i,j)/prlam
          f1   = 4.D0*rrho/3.D0
          f2   = dv(4,i,j)*rrho
          fac  = Max(f1,f2)
          dtv  = fac*fmue/vol(i,j)

          sx       = 0.5D0*(si(1,i,j)+si(1,i+1,j))
          sy       = 0.5D0*(si(2,i,j)+si(2,i+1,j))
          ds2      = sx*sx + sy*sy
          ds       = Sqrt(ds2)
          sx       = sx/ds
          sy       = sy/ds
          vc       = sx*u + sy*v
          cs       = Sqrt((vc*vc)*((a4-1.D0)*(a4-1.D0))+4.D0*a5)
          sri(i,j) = 0.5D0*((a4+1.D0)*Abs(vc)+cs)*ds
          srvi     = dtv*ds2

          sx       = 0.5D0*(sj(1,i,j)+sj(1,i,j+1))
          sy       = 0.5D0*(sj(2,i,j)+sj(2,i,j+1))
          ds2      = sx*sx + sy*sy
          ds       = Sqrt(ds2)
          sx       = sx/ds
          sy       = sy/ds
          vc       = sx*u + sy*v
          cs       = Sqrt((vc*vc)*((a4-1.D0)*(a4-1.D0))+4.D0*a5)
          srj(i,j) = 0.5D0*((a4+1.D0)*Abs(vc)+cs)*ds
          srvj     = dtv*ds2

          tstep(i,j) = vol(i,j)/(sri(i,j)+srj(i,j)+cfac*(srvi+srvj))
        enddo
      enddo

! - original equations

    else
      do j=2,j2
        do i=2,i2
          rrho = 1.D0/cv(1,i,j)
          u    = cv(2,i,j)*rrho
          v    = cv(3,i,j)*rrho
          fmue = dv(6,i,j)/prlam
          f1   = 4.D0*rrho/3.D0
          f2   = dv(4,i,j)*rrho
          fac  = Max(f1,f2)
          dtv  = fac*fmue/vol(i,j)

          sx       = 0.5D0*(si(1,i,j)+si(1,i+1,j))
          sy       = 0.5D0*(si(2,i,j)+si(2,i+1,j))
          ds2      = sx*sx + sy*sy
          vc       = sx*u + sy*v
          cs       = dv(3,i,j)*Sqrt(ds2)
          sri(i,j) = Abs(vc) + cs
          srvi     = dtv*ds2

          sx       = 0.5D0*(sj(1,i,j)+sj(1,i,j+1))
          sy       = 0.5D0*(sj(2,i,j)+sj(2,i,j+1))
          ds2      = sx*sx + sy*sy
          vc       = sx*u + sy*v
          cs       = dv(3,i,j)*Sqrt(ds2)
          srj(i,j) = Abs(vc) + cs
          srvj     = dtv*ds2

          tstep(i,j) = vol(i,j)/(sri(i,j)+srj(i,j)+cfac*(srvi+srvj))
        enddo
      enddo
    endif

  endif ! kequs

! set values of spectral radii in dummy nodes (needed by "DissipCentral") -----

  do i=2,i2
    sri(i,   1) = sri(i, 2)
    sri(i,   0) = sri(i, 2)
    sri(i,  jl) = sri(i,j2)
    sri(i,jmax) = sri(i,j2)
    srj(i,   1) = srj(i, 2)
    srj(i,   0) = srj(i, 2)
    srj(i,  jl) = srj(i,j2)
    srj(i,jmax) = srj(i,j2)
  enddo
  do j=2,j2
    sri(1   ,j) = sri( 2,j)
    sri(0   ,j) = sri( 2,j)
    sri(il  ,j) = sri(i2,j)
    sri(imax,j) = sri(i2,j)
    srj(1   ,j) = srj( 2,j)
    srj(0   ,j) = srj( 2,j)
    srj(il  ,j) = srj(i2,j)
    srj(imax,j) = srj(i2,j)
  enddo

! and now at cuts ...

  do iseg=1,nsegs
    itype = lbsegs(iseg,1)
    lb    = lbsegs(iseg,2)
    lbeg  = lbsegs(iseg,3)
    lend  = lbsegs(iseg,4)
    if (itype>=700 .and. itype<800) then
      lbs   = lbsegs(iseg,5)
      lbegs = lbsegs(iseg,6)
      lends = lbsegs(iseg,7)
      call BcondCutSingle( lb,lbeg,lend,lbs,lbegs,lends,sri )
      call BcondCutSingle( lb,lbeg,lend,lbs,lbegs,lends,srj )
    endif
  enddo

! set coefficients of implicit residual smoothing -----------------------------

  if (ktimst=="L" .and. epsirs>0.D0) then
    cflrat = Sqrt(1.D0+4.D0*epsirs)
    do j=2,j2
      do i=2,i2
        reval  = srj(i,j)/sri(i,j)
        reval1 = 1.D0/reval
        ex     = 0.25D0*((cflrat/(1.D0+0.125D0*reval ))**2 - 1.D0)
        ey     = 0.25D0*((cflrat/(1.D0+0.125D0*reval1))**2 - 1.D0)
        ex     = Min( epsirs, ex )
        ey     = Min( epsirs, ey )
        epsij(1,i,j) = Max( 0.D0, ex )
        epsij(2,i,j) = Max( 0.D0, ey )
      enddo
    enddo
  endif

! find min. time step in domain (if global time-stepping) ---------------------

  if (ktimst == "G") then
    tsmin = 1.D+32
    do j=2,j2
      do i=2,i2
        tsmin = Min(tsmin,tstep(i,j))
      enddo
    enddo
    do j=2,j2
      do i=2,i2
        tstep(i,j) = tsmin
      enddo
    enddo
  endif

end subroutine TimeStep

!> @file bcondFarfield.f90
!!
!! Treatment of far-field boundaries.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: March 7, 2014
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

!> Applies far-field boundary condition to dummy cells. Values are prescribed
!! in the first layer and extrapolated to the second dummy layer. Characteristic
!! boundary conditions are employed in the case of subsonic flow. Conservative
!! variables are inter- / extrapolated in the case of supersonic flow. Vortex
!! correction is optionally applied to the flow variables (subsonic only).
!!
!! @param lb    side of the computational domain (1: j=2, 2: i=i2, 3: j=j2,4: i=2)
!! @param lbeg  start index of the boundary segment
!! @param lend  end index of the boundary segment
!! @param rhof  work array for density at the boundary
!! @param uf    work array for u-velocity at the boundary
!! @param vf    work array for v-velocity at the boundary
!! @param pf    work array for pressure at the boundary
!!
subroutine BcondFarfield( lb,lbeg,lend,rhof,uf,vf,pf )

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  use ModPlotQuant
  use ModInterfaces, only : DependentVarsOne, CompTheta, Forces
  implicit none

! parameters
  integer, intent(in) :: lb, lbeg, lend
  real(rtype) :: rhof(0:), uf(0:), vf(0:), pf(0:)

! local variables
  integer     :: iins1, idum1, idum2, jins1, jdum1, jdum2, lstep
  integer     :: i, j
  real(rtype) :: gmr, gmg, bet, cir, xa, ya, dist, angle, sn, dn, vc, qv2
  real(rtype) :: ds, sxn, syn, rhoe, ue, ve, qqe, pe, qn, crho0, &
                 rhoa, ua, va, pa, ul, vl, pl, sgn, pb, gam1, ggm1
  real(rtype) :: rhop, rhoT, hT, theta, a1, ra1g, a4, a5, cs

! *****************************************************************************

  if      (lb == 1) then
    jins1 = 2
    jdum1 = 1
    jdum2 = 0
  else if (lb == 2) then
    iins1 = i2
    idum1 = il
    idum2 = imax
  else if (lb == 3) then
    jins1 = j2
    jdum1 = jl
    jdum2 = jmax
  else if (lb == 4) then
    iins1 = 2
    idum1 = 1
    idum2 = 0
  endif

                   lstep =  1
  if (lbeg > lend) lstep = -1

! free-stream values (optionally corrected by a vortex) =======================
! values corrected

  if (lvort == "Y") then
    call Forces

    bet = Sqrt(1.D0-machinf*machinf)
    cir = 0.25D0*cref*cl*qinf/pi

    if (lb==1 .or. lb==3) then
      do i=lbeg,lend,lstep
        gam1    = dv(4,i,jins1) - 1.D0
        ggm1    = dv(4,i,jins1)/gam1
        gmr     = 1.D0/dv(4,i,jins1)
        gmg     = gam1/dv(4,i,jins1)
        xa      = x(i,jins1) - xref
        ya      = y(i,jins1) - yref
        dist    = Sqrt(xa*xa+ya*ya)
        angle   = Atan2(ya,xa)
        sn      = Sin(angle-alpha)
        dn      = 1.D0 - machinf*machinf*sn*sn
        vc      = cir*bet/(dn*dist)
        uf(i)   = uinf + vc*Sin(angle)
        vf(i)   = vinf - vc*Cos(angle)
        qv2     = uf(i)*uf(i) + vf(i)*vf(i)
        pf(i)   = (pinf**gmg+gmg*rhoinf*(qinf*qinf-qv2)/(2.D0*pinf**gmr))**ggm1
        rhof(i) = rhoinf*(pf(i)/pinf)**gmr
      enddo
    else if (lb==2 .or. lb==4) then
      do j=lbeg,lend,lstep
        gam1    = dv(4,iins1,j) - 1.D0
        ggm1    = dv(4,iins1,j)/gam1
        gmr     = 1.D0/dv(4,iins1,j)
        gmg     = gam1/dv(4,iins1,j)
        xa      = x(iins1,j) - xref
        ya      = y(iins1,j) - yref
        dist    = Sqrt(xa*xa+ya*ya)
        angle   = Atan2(ya,xa)
        sn      = Sin(angle-alpha)
        dn      = 1.D0 - machinf*machinf*sn*sn
        vc      = cir*bet/(dn*dist)
        uf(j)   = uinf + vc*Sin(angle)
        vf(j)   = vinf - vc*Cos(angle)
        qv2     = uf(j)*uf(j) + vf(j)*vf(j)
        pf(j)   = (pinf**gmg+gmg*rhoinf*(qinf*qinf-qv2)/(2.D0*pinf**gmr))**ggm1
        rhof(j) = rhoinf*(pf(j)/pinf)**gmr
      enddo
    endif

! not corrected

  else
    do i=lbeg,lend,lstep
      rhof(i) = rhoinf
      uf(i)   = uinf
      vf(i)   = vinf
      pf(i)   = pinf
    enddo
  endif

! computation of the boundary values ==========================================
! boundary j=2 / j=j2 and i=lbeg,lend -----------------------------------------

  if (lb==1 .or. lb==3) then

    do i=lbeg,lend,lstep
      if (lb == 1) then
        ds  = Sqrt(sj(1,i,jins1)**2+sj(2,i,jins1)**2)
        sxn = sj(1,i,jins1)/ds
        syn = sj(2,i,jins1)/ds
      else
        ds  = Sqrt(sj(1,i,jdum1)**2+sj(2,i,jdum1)**2)
        sxn = -sj(1,i,jdum1)/ds
        syn = -sj(2,i,jdum1)/ds
      endif
      gam1  = dv(4,i,jins1) - 1.D0
      rhoe  = cv(1,i,jins1)
      ue    = cv(2,i,jins1)/rhoe
      ve    = cv(3,i,jins1)/rhoe
      qqe   = ue*ue + ve*ve
      pe    = dv(1,i,jins1)

      if (machinf < 1.D0) then

! ----- subsonic flow (qn<0: inflow / qn>0: outflow)

        qn = sxn*ue + syn*ve
        if (kprecond == "Y") then
          rhop  =  rhoe/pe
          rhoT  = -rhoe/dv(2,i,jins1)
          hT    =  dv(5,i,jins1)
          theta = CompTheta( dv(4,i,jins1),dv(3,i,jins1),qqe )
          a1    = rhoe*rhop*hT + rhoT
          ra1g  = 1.D0/(rhoe*theta*hT + rhoT)
          a4    = a1*ra1g
          a5    = rhoe*hT*ra1g
          cs    = Sqrt((qn*qn)*((a4-1.D0)*(a4-1.D0))+4.D0*a5)
          crho0 = rhoe*cs
        else
          crho0 = dv(3,i,jins1)*rhoe
        endif

        if (qn < 0.D0) then
          rhoa = rhof(i)
          ua   = uf(i)
          va   = vf(i)
          pa   = pf(i)
          ul   = ue
          vl   = ve
          pl   = pe
          sgn  = -1.D0
          pb   = 0.5D0*(pa+pl-crho0*(sxn*(ua-ul)+syn*(va-vl)))
        else
          rhoa = rhoe
          ua   = ue
          va   = ve
          pa   = pe
          ul   = uf(i)
          vl   = vf(i)
          pl   = pf(i)
          sgn  = +1.D0
          pb   = pf(i)
        endif
        cv(1,i,jdum1) = rhoa + (pb-pa)/(dv(3,i,jins1)**2)
        cv(2,i,jdum1) = cv(1,i,jdum1)*(ua+sgn*sxn*(pa-pb)/crho0)
        cv(3,i,jdum1) = cv(1,i,jdum1)*(va+sgn*syn*(pa-pb)/crho0)
        cv(4,i,jdum1) = pb/gam1 + 0.5D0*(cv(2,i,jdum1)**2+ &
                                         cv(3,i,jdum1)**2)/cv(1,i,jdum1)

      else

! ----- supersonic flow (qn<0: inflow / qn>0: outflow)

        qn = sxn*ue + syn*ve
        if (qn < 0.D0) then
          cv(1,i,jdum1) = rhoinf
          cv(2,i,jdum1) = rhoinf*uinf
          cv(3,i,jdum1) = rhoinf*vinf
          cv(4,i,jdum1) = pinf/gam1 + 0.5D0*rhoinf*qinf*qinf
        else
          cv(1,i,jdum1) = rhoe
          cv(2,i,jdum1) = rhoe*ue
          cv(3,i,jdum1) = rhoe*ve
          cv(4,i,jdum1) = pe/gam1 + 0.5D0*rhoe*qqe
        endif
      endif

      cv(1,i,jdum2) = 2.D0*cv(1,i,jdum1) - cv(1,i,jins1)
      cv(2,i,jdum2) = 2.D0*cv(2,i,jdum1) - cv(2,i,jins1)
      cv(3,i,jdum2) = 2.D0*cv(3,i,jdum1) - cv(3,i,jins1)
      cv(4,i,jdum2) = 2.D0*cv(4,i,jdum1) - cv(4,i,jins1)

      call DependentVarsOne( i,jdum1 )
      call DependentVarsOne( i,jdum2 )
    enddo

! boundary i=2 / i=i2 and j=lbeg,lend -----------------------------------------

  else if (lb==2 .or. lb==4) then

    do j=lbeg,lend,lstep
      if (lb == 4) then
        ds  = Sqrt(si(1,iins1,j)**2+si(2,iins1,j)**2)
        sxn = si(1,iins1,j)/ds
        syn = si(2,iins1,j)/ds
      else
        ds  = Sqrt(si(1,idum1,j)**2+si(2,idum1,j)**2)
        sxn = -si(1,idum1,j)/ds
        syn = -si(2,idum1,j)/ds
      endif
      gam1  = dv(4,iins1,j) - 1.D0
      rhoe  = cv(1,iins1,j)
      ue    = cv(2,iins1,j)/rhoe
      ve    = cv(3,iins1,j)/rhoe
      qqe   = ue*ue + ve*ve
      pe    = dv(1,iins1,j)

      if (machinf < 1.D0) then

! ----- subsonic flow (qn<0: inflow / qn>0: outflow)

        qn = sxn*ue + syn*ve
        if (kprecond == "Y") then
          rhop  =  rhoe/pe
          rhoT  = -rhoe/dv(2,iins1,j)
          hT    =  dv(5,iins1,j)
          theta = CompTheta( dv(4,iins1,j),dv(3,iins1,j),qqe )
          a1    = rhoe*rhop*hT + rhoT
          ra1g  = 1.D0/(rhoe*theta*hT + rhoT)
          a4    = a1*ra1g
          a5    = rhoe*hT*ra1g
          cs    = Sqrt((qn*qn)*((a4-1.D0)*(a4-1.D0))+4.D0*a5)
          crho0 = rhoe*cs
        else
          crho0 = dv(3,iins1,j)*rhoe
        endif

        if (qn < 0.D0) then
          rhoa = rhof(j)
          ua   = uf(j)
          va   = vf(j)
          pa   = pf(j)
          ul   = ue
          vl   = ve
          pl   = pe
          sgn  = -1.D0
          pb   = 0.5D0*(pa+pl-crho0*(sxn*(ua-ul)+syn*(va-vl)))
        else
          rhoa = rhoe
          ua   = ue
          va   = ve
          pa   = pe
          ul   = uf(j)
          vl   = vf(j)
          pl   = pf(j)
          sgn  = +1.D0
          pb   = pl
        endif
        cv(1,idum1,j) = rhoa + (pb-pa)/(dv(3,iins1,j)**2)
        cv(2,idum1,j) = cv(1,idum1,j)*(ua+sgn*sxn*(pa-pb)/crho0)
        cv(3,idum1,j) = cv(1,idum1,j)*(va+sgn*syn*(pa-pb)/crho0)
        cv(4,idum1,j) = pb/gam1 + 0.5D0*(cv(2,idum1,j)**2+ &
                                         cv(3,idum1,j)**2)/cv(1,idum1,j)

      else

! ----- supersonic flow (qn<0: inflow / qn>0: outflow)

        qn = sxn*ue + syn*ve
        if (qn < 0.D0) then
          cv(1,idum1,j) = rhoinf
          cv(2,idum1,j) = rhoinf*uinf
          cv(3,idum1,j) = rhoinf*vinf
          cv(4,idum1,j) = pinf/gam1 + 0.5D0*rhoinf*qinf*qinf
        else
          cv(1,idum1,j) = rhoe
          cv(2,idum1,j) = rhoe*ue
          cv(3,idum1,j) = rhoe*ve
          cv(4,idum1,j) = pe/gam1 + 0.5D0*rhoe*qqe
        endif
      endif

      cv(1,idum2,j) = 2.D0*cv(1,idum1,j) - cv(1,iins1,j)
      cv(2,idum2,j) = 2.D0*cv(2,idum1,j) - cv(2,iins1,j)
      cv(3,idum2,j) = 2.D0*cv(3,idum1,j) - cv(3,iins1,j)
      cv(4,idum2,j) = 2.D0*cv(4,idum1,j) - cv(4,iins1,j)

      call DependentVarsOne( idum1,j )
      call DependentVarsOne( idum2,j )
    enddo

  endif ! lb

end subroutine BcondFarfield

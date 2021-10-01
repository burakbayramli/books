!> @file initSolution.f90
!!
!! Initialization of the flow solution.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: May 24, 2014
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

!> Initializes conservative variables (initial guess). Ideal gas
!! is assumed (constant gas properties used everywhere).
!!
subroutine InitSolution

  use ModDataTypes
  use ModGeometry
  use ModPhysics
  implicit none

! local variables
  integer     :: i, j, ib, ibn, ibegn, iendn
  real(rtype) :: gam1, rgas, xmin, xmax, dx, pinl, temp, rho, &
                 cs, mach, q, dp, dbeta, beta, p, u, v

! *****************************************************************************

  gam1 = gamma - 1.D0
  rgas = gam1*cpgas/gamma

  if (kflow == "E") then

! - external flow

    do i=1,nnodes
      cv(1,i) = rhoinf
      cv(2,i) = rhoinf*uinf
      cv(3,i) = rhoinf*vinf
      cv(4,i) = pinf/gam1 + 0.5D0*rhoinf*qinf*qinf
    enddo

  else

! - internal flow; inlet assumed at xmin, outlet at xmax;
!   flow angle and pressure are linearly interpolated between
!   inlet and outlet

    xmin =  1.D+32
    xmax = -1.D+32
    do i=1,nndint
      xmin = Min(xmin,x(i))
      xmax = Max(xmax,x(i))
    enddo
    dx = xmax - xmin

    pinl  = p12rat*pout
    if (pinl >= ptinl) pinl = 0.99999D0*ptinl  ! otherwise reversed flow at inlet
    dp    = pout - pinl
    dbeta = betaout - betainl
    temp  = ttinl*(pinl/ptinl)**(gam1/gamma)
    rho   = pinl/(rgas*temp)
    cs    = Sqrt(gamma*pinl/rho)
    mach  = Sqrt(2.D0*((ttinl/temp)-1.D0)/gam1)
    q     = mach*cs

    do i=1,nnodes
      beta    = betainl + dbeta*(x(i)-xmin)/dx
      p       = pinl + dp*(x(i)-xmin)/dx
      rho     = p/(rgas*temp)
      u       = q*Cos(beta)
      v       = q*Sin(beta)
      cv(1,i) = rho
      cv(2,i) = rho*u
      cv(3,i) = rho*v
      cv(4,i) = p/gam1 + 0.5D0*rho*q*q
    enddo

  endif

! equalize flow variables at periodic nodes

  ibegn = 1
  do ib=1,nsegs
    iendn = ibound(2,ib)
    if (btype(ib)>=700 .and. btype(ib)<800) then
      do ibn=ibegn,iendn
        i       = bnode(1,ibn)
        j       = bnode(2,ibn)
        cv(1,i) = 0.5D0*(cv(1,i)+cv(1,j))
        cv(2,i) = 0.5D0*(cv(2,i)+cv(2,j))
        cv(3,i) = 0.5D0*(cv(3,i)+cv(3,j))
        cv(4,i) = 0.5D0*(cv(4,i)+cv(4,j))
        cv(1,j) = cv(1,i)
        cv(2,j) = cv(2,i)
        cv(3,j) = cv(3,i)
        cv(4,j) = cv(4,i)
      enddo
    endif
    ibegn = iendn + 1
  enddo

end subroutine InitSolution

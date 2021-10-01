!> @file initSolution.f90
!!
!! Initialization of the flow solution.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: March 6, 2014
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
  integer :: i, j, ii, jj
  real(rtype) :: gam1, rgas, xmin, xmax, dx, pinl, temp, rho, &
                 cs, mach, q, dp, dbeta, beta, p, u, v

! *****************************************************************************

  gam1 = gamma - 1.D0
  rgas = gam1*cpgas/gamma

  if (kflow == "E") then

! external flow

    do j=0,jmax
      do i=0,imax
        cv(1,i,j) = rhoinf
        cv(2,i,j) = rhoinf*uinf
        cv(3,i,j) = rhoinf*vinf
        cv(4,i,j) = pinf/gam1 + 0.5D0*rhoinf*qinf*qinf
      enddo
    enddo

  else

! internal flow; inlet assumed at xmin, outlet at xmax;
! flow angle and pressure are linearly interpolated between
! inlet and outlet

    xmin =  1.D+32
    xmax = -1.D+32
    do j=2,jl
      do i=2,il
        xmin = Min(xmin,x(i,j))
        xmax = Max(xmax,x(i,j))
      enddo
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

    do j=0,jmax
      jj = Max( 2, j)
      jj = Min(jl,jj)
      do i=0,imax
        ii        = Max( 2, i)
        ii        = Min(il,ii)
        beta      = betainl + dbeta*(x(ii,jj)-xmin)/dx
        p         = pinl + dp*(x(ii,jj)-xmin)/dx
        rho       = p/(rgas*temp)
        u         = q*Cos(beta)
        v         = q*Sin(beta)
        cv(1,i,j) = rho
        cv(2,i,j) = rho*u
        cv(3,i,j) = rho*v
        cv(4,i,j) = p/gam1 + 0.5D0*rho*q*q
      enddo
    enddo

  endif

end subroutine InitSolution

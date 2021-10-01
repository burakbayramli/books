!> @file limiterRefvals.f90
!!
!! Initialization of limiter reference values.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: March 10, 2014
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

!> Computes reference values of limited variables (density, u, v, pressure)
!! and of the cell volumes. The reference values are used to normalize variables
!! within the limiter functions (Roe's upwind scheme).
!!
subroutine LimiterRefvals

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  implicit none

! local variables
  real(rtype) :: gam1, rgas, temp, rho, cs, mach

! *****************************************************************************
! reference volume (= maximum cell volume)

  volref = Maxval( vol )

! reference density, velocity and pressure

  gam1 = gamma - 1.D0
  rgas = gam1*cpgas/gamma

! external flow

  if (kflow == "E") then
    limref(1) = rhoinf
    limref(2) = Sqrt(uinf*uinf+vinf*vinf)
    limref(3) = limref(2)
    limref(4) = pinf

! internal flow

  else
    temp      = ttinl*(pout/ptinl)**(gam1/gamma)
    rho       = pout/(rgas*temp)
    cs        = Sqrt(gamma*pout/rho)
    mach      = Sqrt(2.D0*((ttinl/temp)-1.D0)/gam1)
    limref(1) = rho
    limref(2) = mach*cs
    limref(3) = limref(2)
    limref(4) = pout
  endif

end subroutine LimiterRefvals

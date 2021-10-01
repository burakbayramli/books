!> @file dependentVars.f90
!!
!! Computation of dependent variables under the assumption of ideal gas
!! with constant properties.
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

!> Computes values of dependent variables (pressure, temperature, speed
!! of sound, specific heat ratio, specific heat coeff. at const. pressure)
!! from conservative variables in all grid cells. Additionally, laminar
!! viscosity and heat conductivity coefficients are computed in the case
!! of viscous flow.
!!
subroutine DependentVarsAll

  use ModDataTypes
  use ModGeometry
  use ModPhysics
  implicit none

! local variables
  integer :: i, j
  real(rtype) :: gam1, rgas, g1cp, rhoq, s1, s2, s12, rat, cppr

! *****************************************************************************

  gam1 = gamma - 1.D0
  rgas = gam1*cpgas/gamma
  g1cp = gam1*cpgas

! Euler equations

  if (kequs == "E") then
    do j=0,jmax
      do i=0,imax
        rhoq      = cv(2,i,j)*cv(2,i,j) + cv(3,i,j)*cv(3,i,j)
        dv(1,i,j) = gam1*(cv(4,i,j)-0.5D0*rhoq/cv(1,i,j))
        dv(2,i,j) = dv(1,i,j)/(rgas*cv(1,i,j))
        dv(3,i,j) = Sqrt(g1cp*dv(2,i,j))
        dv(4,i,j) = gamma
        dv(5,i,j) = cpgas
      enddo
    enddo

! Navier-Stokes equations

  else
    s1   = 110.D0
    s2   = 288.16D0
    s12  = 1.D0 + s1/s2
    cppr = cpgas/prlam
    do j=0,jmax
      do i=0,imax
        rhoq      = cv(2,i,j)*cv(2,i,j) + cv(3,i,j)*cv(3,i,j)
        dv(1,i,j) = gam1*(cv(4,i,j)-0.5D0*rhoq/cv(1,i,j))
        dv(2,i,j) = dv(1,i,j)/(rgas*cv(1,i,j))
        dv(3,i,j) = Sqrt(g1cp*dv(2,i,j))
        dv(4,i,j) = gamma
        dv(5,i,j) = cpgas
        rat       = Sqrt(dv(2,i,j)/s2)*s12/(1.D0+s1/dv(2,i,j))
        dv(6,i,j) = refvisc*rat
        dv(7,i,j) = dv(6,i,j)*cppr
      enddo
    enddo
  endif

end subroutine DependentVarsAll

! =============================================================================

!> Computes values of dependent variables (pressure, temperature, speed
!! of sound, specific heat ratio, specific heat coeff. at const. pressure)
!! from conservative variables at the cell (I,J). Additionally, laminar
!! viscosity and heat conductivity coefficients are computed in the case
!! of viscous flow.
!!
!! @param i cell index in I-direction
!! @param j cell index in J-direction
!!
subroutine DependentVarsOne( i,j )

  use ModDataTypes
  use ModPhysics
  implicit none

! parameters
  integer, intent(in) :: i, j

! local variables
  real(rtype) :: gam1, rgas, g1cp, rhoq, s1, s2, s12, rat

! *****************************************************************************

  gam1 = gamma - 1.D0
  rgas = gam1*cpgas/gamma
  g1cp = gam1*cpgas

! Euler equations

  if (kequs == "E") then
    rhoq      = cv(2,i,j)*cv(2,i,j) + cv(3,i,j)*cv(3,i,j)
    dv(1,i,j) = gam1*(cv(4,i,j)-0.5D0*rhoq/cv(1,i,j))
    dv(2,i,j) = dv(1,i,j)/(rgas*cv(1,i,j))
    dv(3,i,j) = Sqrt(g1cp*dv(2,i,j))
    dv(4,i,j) = gamma
    dv(5,i,j) = cpgas

! Navier-Stokes equations

  else
    s1        = 110.D0
    s2        = 288.16D0
    s12       = 1.D0 + s1/s2
    rhoq      = cv(2,i,j)*cv(2,i,j) + cv(3,i,j)*cv(3,i,j)
    dv(1,i,j) = gam1*(cv(4,i,j)-0.5D0*rhoq/cv(1,i,j))
    dv(2,i,j) = dv(1,i,j)/(rgas*cv(1,i,j))
    dv(3,i,j) = Sqrt(g1cp*dv(2,i,j))
    dv(4,i,j) = gamma
    dv(5,i,j) = cpgas
    rat       = Sqrt(dv(2,i,j)/s2)*s12/(1.D0+s1/dv(2,i,j))
    dv(6,i,j) = refvisc*rat
    dv(7,i,j) = dv(6,i,j)*(cpgas/prlam)
  endif

end subroutine DependentVarsOne

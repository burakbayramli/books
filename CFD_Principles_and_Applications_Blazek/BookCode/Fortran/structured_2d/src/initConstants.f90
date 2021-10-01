!> @file initConstants.f90
!!
!! Initialization of constant values.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: February 27, 2014
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

!> Initializes constants used by the solver.
!!
subroutine InitConstants

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  implicit none

! local variables
  real(rtype) :: gam1, rgas

! *****************************************************************************

  pi  = 4.D0*Atan(1.D0)
  rad = 180.D0/pi

  if (kflow == 'E') then

! - external flow; it is assumed that "gamma" and "cpgas" specified in
!   the input file are valid for the complete far-field boundary

    gam1 = gamma - 1.D0
    rgas = gam1*cpgas/gamma

    alpha   = alpha/rad
    rhoinf  = pinf/(rgas*tinf)
    qinf    = machinf * Sqrt(gam1*cpgas*tinf)
    uinf    = qinf*Cos(alpha)
    vinf    = qinf*Sin(alpha)
    refrho  = rhoinf
    refvel  = qinf
    if (kequs == 'N') then
      refvisc = rhoinf*qinf*cref/renum
    else
      refvisc = 0.D0
    endif

  else

! - internal flow

    betainl = betainl/rad
    betaout = betaout/rad
    if (kequs == 'N') then
      refvisc = refrho*refvel*cref/renum
    else
      refvisc = 0.D0
    endif

  endif

end subroutine InitConstants

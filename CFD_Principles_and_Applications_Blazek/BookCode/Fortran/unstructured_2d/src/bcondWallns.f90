!> @file bcondWallns.f90
!!
!! Treatment of no-slip (viscous) wall boundaries.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: May 28, 2014
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

!> Applies no-slip wall boundary condition. Adiabatic walls
!! only are assumed (velocity components are zeroed out).
!!
!! @param ibegn  indirect pointer to first node of the boundary
!! @param iendn  indirect pointer to last node of the boundary
!!
subroutine BcondWallns( ibegn,iendn )

  use ModDataTypes
  use ModGeometry
  use ModPhysics
  use ModInterfaces, only : DependentVarsOne
  implicit none

! parameters
  integer, intent(in) :: ibegn, iendn

! local variables
  integer :: ib, ibn

! *****************************************************************************

  do ib=ibegn,iendn

    ibn       = bnode(1,ib)   ! boundary node
    cv(2,ibn) = 0.D0
    cv(3,ibn) = 0.D0

    call DependentVarsOne( ibn )

  enddo

end subroutine BcondWallns

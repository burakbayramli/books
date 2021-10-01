!> @file modDataTypes.f90
!!
!! Specifications of data types employed within the solver.
!
! *****************************************************************************
!
!  (c) Jiri Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: February 25, 2014
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

!> Precision of all real variables; length of strings.
!!
module ModDataTypes

  implicit none

  integer, parameter :: chrlen = 256         !< length of strings
  integer, parameter :: rtype  = kind(1.D0)  !< reals with double precision

end module ModDataTypes

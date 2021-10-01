!> @file modGeometry.f90
!!
!! Grid dimensions and geometry; boundary segments.
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

!> Variables related to grid geometry and topology.
!!
module ModGeometry

  use ModDataTypes
  implicit none

  integer :: nci, &  !< number of grid cells in i-direction
             ncj, &  !< number of grid cells in j-direction
             imax, & !< max. dimensions in i-direction (0 <= i <= imax); imax = nci + 3
             jmax, & !< max. dimensions in j-direction (0 <= j <= jmax); jmax = ncj + 3
             il,  &  !< index of the 1st dummy cell in i-direction (right side); il = imax - 1
             jl,  &  !< index of the 1st dummy cell in j-direction (top side); jl = jmax - 1
             i2,  &  !< index of the last physical cell in i-direction; i2 = imax - 2 (physical cells start at index 2)
             j2      !< index of the last physical cell in j-direction; j2 = jmax - 2 (physical cells start at index 2)

  integer :: nsegs  !< total number of boundary segments
  integer, allocatable :: lbsegs(:,:) !< description of boundary segments\n
    !! @details
    !! Meaning of the entries:
    !! @li (*,1) = boundary type (bctype):\n
    !!             100-199 = inflow\n
    !!             200-299 = outflow\n
    !!             300-399 = viscous wall\n
    !!             400-499 = inviscid wall\n
    !!             500-599 = symmetry line\n
    !!             600-699 = far-field\n
    !!             700-799 = coordinate cut or periodic boundary\n
    !!             800-899 = mass injection
    !! @li (*,2) = side of the computational domain (1: j=2, 2: i=i2, 3: j=j2,
    !!             4: i=2; see Fig. 12.2)
    !! @li (*,3) = start index of the segment (given as cell index, NOT node
    !!             index)
    !! @li (*,4) = end index of the segment
    !! @li (*,5) = side of the computational domain where the source (partner)
    !!             segment is located (if 700 <= bctype <= 799)
    !! @li (*,6) = start index of the source segment
    !! @li (*,7) = end index of the source segment

  real(rtype) :: xref, & !< x-coordinate of the reference point
                 yref, & !< y-coordinate of the reference point
                 cref    !< reference length or airfoil chord

  real(rtype), allocatable :: x(:,:),    & !< x-coordinates of grid points
                              y(:,:),    & !< y-coordinates of grid points
                              si(:,:,:), & !< x,y-components of the face vector (n*dS) in i-direction (see Fig. 12.3)
                              sj(:,:,:), & !< x,y-components of the face vector in j-direction
                              vol(:,:)     !< cell volume (= control volume)

end module ModGeometry

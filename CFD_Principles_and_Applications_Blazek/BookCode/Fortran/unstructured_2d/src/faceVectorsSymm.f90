!> @file faceVectorsSymm.f90
!!
!! Correction of face vectors at symmetry boundaries.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: May 22, 2014
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

!> Corrects face vectors of edges which represent the symmetry boundary.
!! The reason is that there must be no component in the direction normal
!! to the boundary for the fluxes to be computed correctly. Subroutine
!! also changes the boundary type to reflect the orientation of the symmetry
!! boundary: 501 for symmetry along x-direction, 502 along y-direction.
!!
!! @param marker  temporary memory for a node marker
!!
subroutine FaceVectorsSymm( marker )

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  implicit none

! parameters
  integer :: marker(:)

! local variables
  integer     :: i, j, ib, ibf, ibn, ie, ibegf, ibegn, iendf, iendn
  real(rtype) :: sx, sy

! *****************************************************************************
! mark nodes at symmetry boundaries

  do i=1,nndint
    marker(i) = -1
  enddo

  ibegf = 1
  ibegn = 1
  do ib=1,nsegs
    iendf = ibound(1,ib)
    iendn = ibound(2,ib)
    if (btype(ib)>=500 .and. btype(ib)<600) then
      sx = 0.D0
      sy = 0.D0
      do ibf=ibegf,iendf
        sx = sx + sbf(1,ibf)
        sy = sy + sbf(2,ibf)
      enddo
      if (Abs(sx) > Abs(sy)) then
        btype(ib) = 501      ! symmetry at x=const. plane
      else
        btype(ib) = 502      ! symmetry at y=const. plane
      endif
      do ibn=ibegn,iendn
        marker(bnode(1,ibn)) = btype(ib) - 500    ! store symmetry axis
      enddo
    endif
    ibegf = iendf + 1
    ibegn = iendn + 1
  enddo

! correct face vectors

  do ie=1,nedint
    i = edge(1,ie)
    j = edge(2,ie)
    if (marker(i)/=-1 .and. marker(j)/=-1) then
      if (marker(i) < 2) then     ! x=const. plane
        sij(1,ie) = 0.D0
      else                        ! y=const. plane
        sij(2,ie) = 0.D0
      endif
    endif
  enddo

end subroutine FaceVectorsSymm

!> @file zeroResiduals.f90
!!
!! Correction of residuals at symmetry and no-slip boundaries.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: June 3, 2014
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

!> Zeros out normal component of the residual at symmetry and
!! at no-slip boundaries.
!!
subroutine ZeroResiduals

  use ModGeometry
  use ModNumerics
  use ModPhysics
  implicit none

! local variables
  integer :: i, ib, ibn, idir, ibegn, iendn

! *****************************************************************************

  ibegn = 1

  do ib=1,nsegs
    iendn = ibound(2,ib)

! - symmetry boundary

    if (btype(ib)>=500 .and. btype(ib)<600) then
      if (btype(ib)-500 <  2) idir = 2  ! x=const. line -> x-component
      if (btype(ib)-500 >= 2) idir = 3  ! y=const. line -> y-component
      do ibn=ibegn,iendn
        i           = bnode(1,ibn)
        rhs(idir,i) = 0.D0
      enddo

! - viscous (no-slip) wall

    else if ((btype(ib)>=300 .and. btype(ib)<400) .and. kequs=="N") then
      do ibn=ibegn,iendn
        i        = bnode(1,ibn)
        rhs(2,i) = 0.D0       ! velocity components = 0
        rhs(3,i) = 0.D0
      enddo
    endif

    ibegn = iendn + 1
  enddo

end subroutine ZeroResiduals

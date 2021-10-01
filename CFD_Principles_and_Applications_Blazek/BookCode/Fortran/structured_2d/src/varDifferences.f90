!> @file varDifferences.f90
!!
!! Differences of primitive variables.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: March 24, 2014
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

!> Computes differences between primitive variables (rho, u, v, and p here).
!! This subroutine is utilized for higher-order Roe's upwind scheme (for
!! limited extrapolation to the cell faces).
!!
subroutine VarDifferences

  use ModGeometry
  use ModNumerics
  use ModPhysics
  use ModInterfaces, only : BcondCut
  implicit none

! local variables
  integer :: itype, lb, lbeg, lend, lbs, lbegs, lends
  integer :: i, j, iseg

! *****************************************************************************
! differences in i-direction (at I-1/2)

  do j=2,j2
    do i=1,imax
      dui(1,i,j) = cv(1,i,j)           - cv(1,i-1,j)
      dui(2,i,j) = cv(2,i,j)/cv(1,i,j) - cv(2,i-1,j)/cv(1,i-1,j)
      dui(3,i,j) = cv(3,i,j)/cv(1,i,j) - cv(3,i-1,j)/cv(1,i-1,j)
      dui(4,i,j) = dv(1,i,j)           - dv(1,i-1,j)
    enddo
    dui(1,0,j) = dui(1,1,j)
    dui(2,0,j) = dui(2,1,j)
    dui(3,0,j) = dui(3,1,j)
    dui(4,0,j) = dui(4,1,j)
  enddo

! differences in j-direction (at J-1/2)

  do i=2,i2
    do j=1,jmax
      duj(1,i,j) = cv(1,i,j)           - cv(1,i,j-1)
      duj(2,i,j) = cv(2,i,j)/cv(1,i,j) - cv(2,i,j-1)/cv(1,i,j-1)
      duj(3,i,j) = cv(3,i,j)/cv(1,i,j) - cv(3,i,j-1)/cv(1,i,j-1)
      duj(4,i,j) = dv(1,i,j)           - dv(1,i,j-1)
    enddo
    duj(1,i,0) = duj(1,i,1)
    duj(2,i,0) = duj(2,i,1)
    duj(3,i,0) = duj(3,i,1)
    duj(4,i,0) = duj(4,i,1)
  enddo

! treatment of cuts / periodic boundaries

  do iseg=1,nsegs
    itype = lbsegs(iseg,1)
    lb    = lbsegs(iseg,2)
    lbeg  = lbsegs(iseg,3)
    lend  = lbsegs(iseg,4)
    if (itype>=700 .and. itype<800) then
      lbs   = lbsegs(iseg,5)
      lbegs = lbsegs(iseg,6)
      lends = lbsegs(iseg,7)
      call BcondCut( lb,lbeg,lend,lbs,lbegs,lends,4,dui )
      call BcondCut( lb,lbeg,lend,lbs,lbegs,lends,4,duj )
    endif
  enddo

end subroutine VarDifferences

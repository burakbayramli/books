!> @file periodic.f90
!!
!! Addition of variables at periodic boundaries.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: May 23, 2014
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

!> Adds both parts of variable var(:,:) at all periodic boundaries.
!!
!! @param var  variable to update
!!
subroutine Periodic( var )

  use ModDataTypes
  use ModGeometry
  implicit none

! parameters
  real(rtype) :: var(:,:)

! local variables
  integer :: i, j, ib, ibn, ibegn, iendn, n

! *****************************************************************************

  ibegn = 1
  do ib=1,nsegs
    iendn = ibound(2,ib)
    if (btype(ib)>=700 .and. btype(ib)<800) then
      do n=1,Ubound(var,1)
        do ibn=ibegn,iendn
          i        = bnode(1,ibn)
          j        = bnode(2,ibn)
          var(n,i) = var(n,i) + var(n,j)
          var(n,j) = var(n,i)
        enddo
      enddo
    endif
    ibegn = iendn + 1
  enddo

end subroutine Periodic

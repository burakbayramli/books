!> @file readGrid.f90
!!
!! Input of grid coordinates.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: March 5, 2014
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

!> Reads in coordinates of the grid nodes.
!!
subroutine ReadGrid

  use ModFiles
  use ModGeometry
  use ModInterfaces, only : ErrorMessage
  implicit none

! local variables
  integer :: errFlag, i, j, nciDum, ncjDum

! *****************************************************************************

  open(unit=ifGrid, file=fnGrid, status="old", action="read", iostat=errFlag)
  if (errFlag /= 0) call ErrorMessage( "cannot open grid file" )

  read(ifGrid,"(1X)")
  read(ifGrid,"(1X)")
  read(ifGrid,"(1X)")

! dimensions (for checking purposes)

  read(ifGrid,*) nciDum,ncjDum
  if (nciDum/=nci .or. ncjDum/=ncj) &
    call ErrorMessage( "grid dimensions differ from the topology file" )

! coordinates

  read(ifGrid,"(1X)")
  read(ifGrid,*) ((x(i,j),y(i,j), i=2,il), j=2,jl)

  close(unit=ifGrid)

! grid points for dummy cells

  do i=2,il
    x(i,0   ) = x(i,2 )
    y(i,0   ) = y(i,2 )
    x(i,1   ) = x(i,2 )
    y(i,1   ) = y(i,2 )
    x(i,jmax) = x(i,jl)
    y(i,jmax) = y(i,jl)
  enddo
  do j=0,jmax
    x(0   ,j) = x(2 ,j)
    y(0   ,j) = y(2 ,j)
    x(1   ,j) = x(2 ,j)
    y(1   ,j) = y(2 ,j)
    x(imax,j) = x(il,j)
    y(imax,j) = y(il,j)
  enddo

end subroutine ReadGrid

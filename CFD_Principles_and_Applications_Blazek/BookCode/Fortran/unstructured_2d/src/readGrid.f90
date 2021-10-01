!> @file readGrid.f90
!!
!! Input of grid coordinates, elements and boundaries.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: May 21, 2014
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

!> Reads in grid data and boundary segments.
!!
subroutine ReadGrid

  use ModFiles
  use ModGeometry
  use ModInterfaces, only : DummyNodes, ErrorMessage
  implicit none

! local variables
  integer :: errFlag, i, ib, ibn, ibf, ibegf, iendf, ibegn, iendn

! *****************************************************************************

  open(unit=ifGrid, file=fnGrid, status="old", action="read", iostat=errFlag)
  if (errFlag /= 0) call ErrorMessage( "cannot open grid file" )

  read(ifGrid,"(1X)")
  read(ifGrid,"(1X)")
  read(ifGrid,"(1X)")

! numbers of physical nodes, triangles and boundary segments

  read(ifGrid,*) nndint,ntria,nsegs

! boundary type, no. of boundary faces & nodes, boundary name

  allocate( btype(nsegs),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for btype()" )

  allocate( bname(nsegs),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for bname()" )

  allocate( ibound(2,nsegs),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for ibound()" )

  read(ifGrid,"(1X)")
  do ib=1,nsegs
    read(ifGrid,  *  ) btype(ib),ibound(1,ib),ibound(2,ib)
    read(ifGrid,"(A)") bname(ib)
  enddo

  nbfaces = ibound(1,nsegs)
  nbnodes = ibound(2,nsegs)

! definition of boundary faces / periodic nodes

  allocate( bnode(3,nbnodes),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for bnode()" )

  allocate( bface(2,nbfaces),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for bface()" )

  do ibn=1,nbnodes
    bnode(1,ibn) = -777
    bnode(2,ibn) = -777      ! set in DummyNodes
    bnode(3,ibn) = -777      ! set in EdgesFinalize
  enddo
  do ibf=1,nbfaces
    bface(1,ibf) = -777
    bface(2,ibf) = -777
  enddo

  read(ifGrid,"(1X)")
  ibegf = 1
  ibegn = 1
  do ib=1,nsegs
    iendf = ibound(1,ib)
    iendn = ibound(2,ib)
    if (btype(ib)>=700 .and. btype(ib)<800) then   ! periodic nodes
      do ibn=ibegn,iendn
        read(ifGrid,*) bnode(1,ibn),bnode(2,ibn)
      enddo
    else                                           ! boundary faces
      do ibf=ibegf,iendf
        read(ifGrid,*) bface(1,ibf),bface(2,ibf)
      enddo
    endif
    ibegf = iendf + 1
    ibegn = iendn + 1
  enddo

! check boundary faces pointer

  do ibf=1,nbfaces
    if (bface(1,ibf)<0 .or. bface(2,ibf)<0) then
      call ErrorMessage( "array bface() not completely defined" )
    endif
  enddo

! generate dummy nodes

  call DummyNodes

! grid nodes

  allocate( x(nnodes),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for x()" )

  allocate( y(nnodes),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for y()" )

  read(ifGrid,"(1X)")
  do i=1,nndint
    read(ifGrid,*) x(i),y(i)
  enddo

! triangles

  allocate( tria(3,ntria),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for tria()" )

  read(ifGrid,"(1X)")
  do i=1,ntria
    read(ifGrid,*) tria(1,i),tria(2,i),tria(3,i)
  enddo

  close(unit=ifGrid)

end subroutine ReadGrid

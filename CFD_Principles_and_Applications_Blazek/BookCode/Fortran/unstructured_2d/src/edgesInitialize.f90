!> @file edgesInitialize.f90
!!
!! Generation of temporary edge lists.
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

!> Generates temporary lists with nodes of an edge (niedge, iedge).
!! Computes total number of edges (interior + dummy). The edge lists
!! are used in the subroutines EdgesFinalize and InitMetrics.
!!
!! @param niedge  pointer from a node to iedge()
!! @param iedge   linked list of edge endpoints:
!!                @li (1,*) = point j of edge (i,j)
!!                @li (2,*) = next point j which is also connected to i;
!!                            if <0 - no further connections
!!                @li (3,*) = pointer to edge() - used in InitMetrics to associate
!!                            face vector sij() with the correct edge
!!
subroutine EdgesInitialize( niedge,iedge )

  use ModGeometry
  use ModNumerics
  use ModInterfaces, only : ErrorMessage
  implicit none

! parameters
  integer, intent(out) :: niedge(:), iedge(:,:)

! local variables
  integer :: d, cedge, cedge2, mxedges
  integer :: i, j, ic, ie, n

! *****************************************************************************

  mxedges = ubound(iedge,2)  ! max. possible number of edges

! reset all pointers

  do i=1,nndint
    niedge(i) = -777
  enddo
  do ie=1,mxedges
    iedge(1,ie) = -777
    iedge(2,ie) = -777
    iedge(3,ie) = -777
  enddo

! loop over nodes of all triangles

  nedint = 0

  do n=1,3

! - loop over triangles

    do ic=1,ntria

      i = tria(n,ic)
      if (n < 3) then
        j = tria(n+1,ic)
      else
        j = tria(1,ic)
      endif
      if (i > j) then  ! lower index first
        d = i
        i = j
        j = d
      endif

      if (niedge(i) < 0) then

! ----- define new edge

        nedint = nedint + 1
        if (nedint > mxedges) then
          call ErrorMessage( "max. number of edges reached" )
        endif
        niedge(i)       = nedint
        iedge(1,nedint) = j

      else

! ----- insert node "j" into list of adjacent nodes

        cedge = niedge(i)
10      continue
          if (iedge(1,cedge) == j) goto 20
          cedge2 = iedge(2,cedge)
          if (cedge2 < 0) then
            nedint = nedint + 1
            if (nedint > mxedges) then
              call ErrorMessage( "max. number of edges reached" )
            endif
            iedge(2,cedge ) = nedint
            iedge(1,nedint) = j
            goto 20
          endif
          cedge = cedge2
        goto 10
20      continue
      endif

    enddo ! loop over triangles

  enddo   ! loop over nodes of triangles

! set total no. of edges (add edges to dummy nodes)

  nedges = nedint + (nnodes-nndint)

end subroutine EdgesInitialize

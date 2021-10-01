!> @file initMetrics.f90
!!
!! Initialization of grid metrics.
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

!> Initializes grid metrics: computes face vectors and cell volumes.
!!
!! @param niedge  pointer from a node to iedge()
!! @param iedge   linked list of edge endpoints:
!!                @li (1,*) = point j of edge (i,j)
!!                @li (2,*) = next point j which is also connected to i;
!!                            if <0 - no further connections
!!                @li (3,*) = pointer to edge() - used to associate face
!!                            vector sij() with the correct edge
!!
subroutine InitMetrics( niedge,iedge )

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModInterfaces, only : ErrorMessage
  implicit none

! parameters
  integer, intent(in) :: niedge(:), iedge(:,:)

! local variables
  integer     :: cedge, d, errFlag, i, j, ic, ie, n
  real(rtype) :: x1, y1, x2, y2, x3, y3, area, pvol, cx, cy, sx, sy, vprod

! *****************************************************************************
! allocate memory

  allocate( sij(2,nedges),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for sij()" )

  allocate( vol(nnodes),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for vol()" )

! zero out the variables

  do i=1,nnodes
    vol(i) = 0.D0
  enddo
  do ie=1,nedges
    sij(1,ie) = 0.D0
    sij(2,ie) = 0.D0
  enddo

! loop over triangles, compute volumes and face vectors

  do ic=1,ntria

! - compute triangle area

    x1 = x(tria(1,ic))
    y1 = y(tria(1,ic))
    x2 = x(tria(2,ic))
    y2 = y(tria(2,ic))
    x3 = x(tria(3,ic))
    y3 = y(tria(3,ic))

    area = 0.5D0*((x1-x2)*(y1+y2)+(x2-x3)*(y2+y3)+(x3-x1)*(y3+y1))
    pvol = Abs(area)/3.D0

! - distribute area to the corner nodes (1/3 to each)

    vol(tria(1,ic)) = vol(tria(1,ic)) + pvol
    vol(tria(2,ic)) = vol(tria(2,ic)) + pvol
    vol(tria(3,ic)) = vol(tria(3,ic)) + pvol

! - compute center of the triangle

    cx = (x1+x2+x3)/3.D0
    cy = (y1+y2+y3)/3.D0

! - loop over individual nodes

    do n=1,3

      i = tria(n,ic)
      if (n < 3) then
        j = tria(n+1,ic)
      else
        j = tria(1,ic)
      endif
      if (i > j) then   ! lower index first
        d = i
        i = j
        j = d
      endif

! --- compute part of face vector associated with edge ij;
!     orient it to point in direction from i to j

      sx =  cy - 0.5D0*(y(i)+y(j))
      sy = -cx + 0.5D0*(x(i)+x(j))

      vprod = sx*(x(j)-x(i)) + sy*(y(j)-y(i))
      if (vprod < 0.D0) then
        sx = -sx
        sy = -sy
      endif

! --- search corresponding edge and add (sx,sy) to sij()

      cedge = niedge(i)
10    continue
        if (iedge(1,cedge) == j) then
          ie        = iedge(3,cedge)
          sij(1,ie) = sij(1,ie) + sx
          sij(2,ie) = sij(2,ie) + sy
          goto 20
        endif
        cedge = iedge(2,cedge)
        if (cedge < 0) then
          call ErrorMessage( "could not find edge to a node" )
        endif
      goto 10
20    continue

    enddo   ! loop over nodes of a triangle

  enddo     ! loop over triangles

end subroutine InitMetrics

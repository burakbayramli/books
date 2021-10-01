!> @file modGeometry.f90
!!
!! Grid dimensions and geometry; boundary segments.
!
! *****************************************************************************
!
!  (c) Jiri Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: June 2, 2014
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

  integer :: nnodes !< number of grid nodes (including dummy nodes)\n
    !! @details
    !! Dummy nodes are defined for inlet, outlet and far-field boundaries only. The
    !! variables cv(), dv(), x(), y(), edge() and sij() are divided into two parts.
    !! The first part is related to the physical grid (dimensions nndint, nedint). The
    !! second part refers to the dummy nodes. This approach makes it easier to loop
    !! either only over the physical grid nodes (edges), or over all nodes (edges)
    !! using the same vector.
  integer :: nndint, &  !< number of physical grid nodes (nnodes - dummy nodes)
             ntria, &   !< number of triangles
             nsegs, &   !< number of boundary segments (composed of boundary faces)
             nbfaces, & !< number of all boundary faces
             nbnodes    !< total number of boundary nodes

  character(chrlen), allocatable :: bname(:) !< names of boundary segments (used for plots)

  integer, allocatable :: btype(:) !< types of boundary conditions:\n
    !! 100-199 = inflow\n
    !! 200-299 = outflow\n
    !! 300-399 = viscous wall\n
    !! 400-499 = inviscid wall\n
    !! 500-599 = symmetry line\n
    !! 600-699 = far-field\n
    !! 700-799 = periodic boundary
  integer, allocatable :: bface(:,:) !< indexes of two nodes defining a face (NOT used for periodic boundaries!)
  integer, allocatable :: bnode(:,:) !< data related to boundary nodes\n
    !! @details
    !! Meaning of the entries:
    !! @li (1,*) = index of the node itself
    !! @li (2,*) = index of the related dummy node (inlet, outlet, far-field)
    !!             or of the other periodic node; otherwise = -777
    !! @li (3,*) = index of edge to the dummy node (edge(*,ie), ie > nedint)
  integer, allocatable :: ibound(:,:) !< pointer from boundary segment to boundary faces and nodes\n
    !! @details
    !! Meaning of the entries:
    !! @li (1,*) = last index in bface()
    !! @li (2,*) = last index in bnode()

  real(rtype) :: xref, & !< x-coordinate of the reference point
                 yref, & !< y-coordinate of the reference point
                 cref    !< reference length or airfoil chord

  integer, allocatable :: tria(:,:) !< node indexes of triangle elements

  real(rtype), allocatable :: x(:), &  !< x-coordinates of grid points
                              y(:)     !< y-coordinates of grid points
  real(rtype), allocatable :: sij(:,:) !< x,y-components of the face vector (n*dS)\n
    !! @details
    !! For ie > nedint, sij(*,ie) represents the average face vector at a
    !! boundary node (face between boundary and dummy node);
    !! sij() always points from node i to node j (see Fig. 5.9).
  real(rtype), allocatable :: vol(:)   !< median-dual control volume (shaded area in Fig. 5.8)
  real(rtype), allocatable :: sbf(:,:) !< normal vector of boundary face (outward pointing,
    !! size equal to the edge length); defined for all boundaries except for periodic ones
  real(rtype), allocatable :: sproj(:,:) !< projections of control volumes on the x- and y-axis
    !! (used to compute the time step - see Eq. (6.22))

end module ModGeometry

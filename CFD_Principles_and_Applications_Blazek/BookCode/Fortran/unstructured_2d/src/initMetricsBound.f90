!> @file initMetricsBound.f90
!!
!! Initialization of grid metrics at the boundaries.
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

!> Duplicates control volumes at periodic boundaries. Initializes face
!! vectors at boundaries (except at periodic boundaries). Computes also
!! face vectors for dummy nodes (inlet/outlet/far-field). Sets coordinates
!! of dummy nodes equal to those of boundary nodes.
!!
!! @param marker  temporary node marker
!! @param btria   temporary pointer from boundary face to triangle
!!
subroutine InitMetricsBound( marker,btria )

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModInterfaces, only : ErrorMessage
  implicit none

! parameters
  integer :: marker(:), btria(:,:)

! local variables
  integer     :: d, errFlag, i, j, n1, n2, ibegf, ibegn, iendf, iendn, itype
  integer     :: ib, ibf, ibn, ic, ie, n
  logical     :: flag
  real(rtype) :: cx, cy, xm, ym, vprod

! *****************************************************************************
! combine control volumes at periodic nodes -----------------------------------

  ibegn = 1
  do ib=1,nsegs
    iendn = ibound(2,ib)
    if (btype(ib)>=700 .and. btype(ib)<800) then
      do ibn=ibegn,iendn
        i      = bnode(1,ibn)
        j      = bnode(2,ibn)
        vol(i) = vol(i) + vol(j)
        vol(j) = vol(i)
      enddo
    endif
    ibegn = iendn + 1
  enddo

! compute face vectors at the boundaries --------------------------------------
! (find for each boundary face the corresp. triangle - required in order
! to define the face vectors such as to point OUT of the domain)

! reset node marker (nodes touched by bface()); mark nodes

  do i=1,nndint
    marker(i) = -1
  enddo

  do ibf=1,nbfaces
    marker(bface(1,ibf)) = 1
    marker(bface(2,ibf)) = 1
  enddo

! loop over triangles - if a face = bface() then store index of
! triangle in btria(1,*) => pointer from boundary face to triangle

  do ibf=1,nbfaces
    btria(1,ibf) = -1       ! empty now
  enddo

  do n=1,3                  ! loop over nodes of the triangles

    do ic=1,ntria
      i = tria(n,ic)
      if (n < 3) then       ! i, j define face of a triangle
        j = tria(n+1,ic)
      else
        j = tria(1,ic)
      endif
      if (i > j) then       ! lower index first
        d = i
        i = j
        j = d
      endif
      if (marker(i)==1 .and. marker(j)==1) then   ! must be on boundary
        do ibf=1,nbfaces                  ! search thru all boundary faces
          if (btria(1,ibf) == -1) then    ! triangle unknown
            n1 = bface(1,ibf)             ! n1, n2 define boundary face
            n2 = bface(2,ibf)
            if (n1 > n2) then             ! lower index first
              d  = n1
              n1 = n2
              n2 = d
            endif
            if (i==n1 .and. j==n2) btria(1,ibf) = ic  ! triangle found
          endif
        enddo
      endif
    enddo  ! loop over triangles

  enddo    ! loop over triangle nodes

! check if pointers from boundary faces to triangles valid

  do ibf=1,nbfaces
    if (btria(1,ibf) < 0) then
      call ErrorMessage( "invalid pointer from boundary face to triangle" )
    endif
  enddo

! allocate and compute boundary face vector sbf

  allocate( sbf(2,nbfaces),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for sbf()" )

  do ibf=1,nbfaces
    sbf(1,ibf) = y(bface(2,ibf)) - y(bface(1,ibf))
    sbf(2,ibf) = x(bface(1,ibf)) - x(bface(2,ibf))

! - change orientation of sbf (must be outward facing)

    ic    = btria(1,ibf)
    cx    = (x(tria(1,ic))+x(tria(2,ic))+x(tria(3,ic)))/3.D0
    cy    = (y(tria(1,ic))+y(tria(2,ic))+y(tria(3,ic)))/3.D0
    xm    = 0.5D0*(x(bface(1,ibf))+x(bface(2,ibf)))
    ym    = 0.5D0*(y(bface(1,ibf))+y(bface(2,ibf)))
    vprod = sbf(1,ibf)*(xm-cx) + sbf(2,ibf)*(ym-cy)
    if (vprod < 0.D0) then
      sbf(1,ibf) = -sbf(1,ibf)
      sbf(2,ibf) = -sbf(2,ibf)
    endif
  enddo

! generate face vectors for dummy edges ---------------------------------------

  ibegf = 1
  ibegn = 1
  do ib=1,nsegs
    iendf = ibound(1,ib)
    iendn = ibound(2,ib)
    itype = btype(ib)
    flag  = .false.
    if (itype>=100 .and. itype<200) flag = .true.
    if (itype>=200 .and. itype<300) flag = .true.
    if (itype>=600 .and. itype<700) flag = .true.
    if (flag) then                      ! inlet/outlet/far-field boundary
      do n=1,2                          ! loop over nodes of boundary faces
        do ibf=ibegf,iendf              ! loop over boundary faces
          n1 = bface(n,ibf)             ! which node
          do i=ibegn,iendn              ! search for corresp. boundary node
            if (bnode(1,i) == n1) then
              ie        = bnode(3,i)    ! edge to dummy node
              sij(1,ie) = sij(1,ie) + 0.5D0*sbf(1,ibf)
              sij(2,ie) = sij(2,ie) + 0.5D0*sbf(2,ibf)
            endif
          enddo       ! i
        enddo         ! ibf
      enddo           ! n
    endif             ! flag
    ibegf = iendf + 1
    ibegn = iendn + 1
  enddo

! set coordinates and volumes of dummy nodes ----------------------------------

  do ie=nedint+1,nedges
    i      = edge(1,ie)        ! boundary node
    j      = edge(2,ie)        ! dummy node
    x(j)   = x(i)
    y(j)   = y(i)
    vol(j) = vol(i)
  enddo

end subroutine InitMetricsBound

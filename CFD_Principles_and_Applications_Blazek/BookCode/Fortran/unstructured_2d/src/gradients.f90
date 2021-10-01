!> @file gradients.f90
!!
!! Computation of gradients (rho, u, v, p) at grid nodes.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: June 4, 2014
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

!> Computes gradients of the density, u, v, and of the pressure with respect
!! to the x- and y-coordinates. Gradients are evaluated at the grid nodes.
!!
subroutine Gradients

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  use ModInterfaces, only : Periodic
  implicit none

! local variables
  integer     :: i, j, ib, ibf, ibn, ie, ibegf, iendf, ibegn, iendn
  logical     :: flag
  real(rtype) :: rav, uav, vav, pav, sx, sy
  real(rtype) :: fcx(4), fcy(4)

! *****************************************************************************
! initialize gradients to zero

  do i=1,nnodes
    gradx(1,i) = 0.D0
    gradx(2,i) = 0.D0
    gradx(3,i) = 0.D0
    gradx(4,i) = 0.D0

    grady(1,i) = 0.D0
    grady(2,i) = 0.D0
    grady(3,i) = 0.D0
    grady(4,i) = 0.D0
  enddo

! sum up contributions at edge endpoints --------------------------------------

  do ie=1,nedint
    i = edge(1,ie)
    j = edge(2,ie)

! - average of variables

    rav = 0.5D0*(cv(1,i)+cv(1,j))
    uav = 0.5D0*(cv(2,i)/cv(1,i)+cv(2,j)/cv(1,j))
    vav = 0.5D0*(cv(3,i)/cv(1,i)+cv(3,j)/cv(1,j))
    pav = 0.5D0*(dv(1,i)+dv(1,j))

! - gradients (divided later by the volume)

    fcx(1) = rav*sij(1,ie)
    fcx(2) = uav*sij(1,ie)
    fcx(3) = vav*sij(1,ie)
    fcx(4) = pav*sij(1,ie)

    fcy(1) = rav*sij(2,ie)
    fcy(2) = uav*sij(2,ie)
    fcy(3) = vav*sij(2,ie)
    fcy(4) = pav*sij(2,ie)

    gradx(1,i) = gradx(1,i) + fcx(1)
    gradx(2,i) = gradx(2,i) + fcx(2)
    gradx(3,i) = gradx(3,i) + fcx(3)
    gradx(4,i) = gradx(4,i) + fcx(4)
    gradx(1,j) = gradx(1,j) - fcx(1)
    gradx(2,j) = gradx(2,j) - fcx(2)
    gradx(3,j) = gradx(3,j) - fcx(3)
    gradx(4,j) = gradx(4,j) - fcx(4)

    grady(1,i) = grady(1,i) + fcy(1)
    grady(2,i) = grady(2,i) + fcy(2)
    grady(3,i) = grady(3,i) + fcy(3)
    grady(4,i) = grady(4,i) + fcy(4)
    grady(1,j) = grady(1,j) - fcy(1)
    grady(2,j) = grady(2,j) - fcy(2)
    grady(3,j) = grady(3,j) - fcy(3)
    grady(4,j) = grady(4,j) - fcy(4)
  enddo

! contributions from the boundaries -------------------------------------------

  ibegf = 1

  do ib=1,nsegs
    iendf = ibound(1,ib)
    flag  = .true.
    if (btype(ib)>=500 .and. btype(ib)<600) flag = .false.
    if (btype(ib)>=700 .and. btype(ib)<800) flag = .false.
    if (flag) then     ! all except symmetry and periodic boundaries
      do ibf=ibegf,iendf
        i   = bface(1,ibf)
        j   = bface(2,ibf)
        sx  = sbf(1,ibf)/12.D0
        sy  = sbf(2,ibf)/12.D0

! ----- node i

        rav = 5.D0*cv(1,i)         + cv(1,j)
        uav = 5.D0*cv(2,i)/cv(1,i) + cv(2,j)/cv(1,j)
        vav = 5.D0*cv(3,i)/cv(1,i) + cv(3,j)/cv(1,j)
        pav = 5.D0*dv(1,i)         + dv(1,j)

        gradx(1,i) = gradx(1,i) + rav*sx
        gradx(2,i) = gradx(2,i) + uav*sx
        gradx(3,i) = gradx(3,i) + vav*sx
        gradx(4,i) = gradx(4,i) + pav*sx

        grady(1,i) = grady(1,i) + rav*sy
        grady(2,i) = grady(2,i) + uav*sy
        grady(3,i) = grady(3,i) + vav*sy
        grady(4,i) = grady(4,i) + pav*sy

! ----- node j

        rav = 5.D0*cv(1,j)         + cv(1,i)
        uav = 5.D0*cv(2,j)/cv(1,j) + cv(2,i)/cv(1,i)
        vav = 5.D0*cv(3,j)/cv(1,j) + cv(3,i)/cv(1,i)
        pav = 5.D0*dv(1,j)         + dv(1,i)

        gradx(1,j) = gradx(1,j) + rav*sx
        gradx(2,j) = gradx(2,j) + uav*sx
        gradx(3,j) = gradx(3,j) + vav*sx
        gradx(4,j) = gradx(4,j) + pav*sx

        grady(1,j) = grady(1,j) + rav*sy
        grady(2,j) = grady(2,j) + uav*sy
        grady(3,j) = grady(3,j) + vav*sy
        grady(4,j) = grady(4,j) + pav*sy
      enddo
    endif
    ibegf = iendf + 1
  enddo

! correct at symmetry boundaries

  ibegn = 1

  do ib=1,nsegs
    iendn = ibound(2,ib)
    if (btype(ib)>=500 .and. btype(ib)<600) then
      if (btype(ib)-500 < 2) then    ! x=const. line
        do ibn=ibegn,iendn
          i          = bnode(1,ibn)
          gradx(1,i) = 0.D0
          grady(2,i) = 0.D0
          gradx(3,i) = 0.D0
          gradx(4,i) = 0.D0
        enddo
      else                           ! y=const. line
        do ibn=ibegn,iendn
          i          = bnode(1,ibn)
          grady(1,i) = 0.D0
          grady(2,i) = 0.D0
          gradx(3,i) = 0.D0
          grady(4,i) = 0.D0
        enddo
      endif
    endif
    ibegn = iendn + 1
  enddo

! sum up at periodic boundaries

  call Periodic( gradx )
  call Periodic( grady )

! divide by the control volume ------------------------------------------------

  do i=1,nndint
    gradx(1,i) = gradx(1,i)/vol(i)
    gradx(2,i) = gradx(2,i)/vol(i)
    gradx(3,i) = gradx(3,i)/vol(i)
    gradx(4,i) = gradx(4,i)/vol(i)

    grady(1,i) = grady(1,i)/vol(i)
    grady(2,i) = grady(2,i)/vol(i)
    grady(3,i) = grady(3,i)/vol(i)
    grady(4,i) = grady(4,i)/vol(i)
  enddo

end subroutine Gradients

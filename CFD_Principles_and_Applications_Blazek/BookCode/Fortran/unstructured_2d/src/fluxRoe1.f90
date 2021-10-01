!> @file fluxRoe1.f90
!!
!! Computation of the convective fluxes based on flux averages.
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

!> Computes the convective fluxes using average of fluxes at control volume's
!! faces. The left and right fluxes are computed directly from values at nodes
!! "i" and "j". This is sufficient for a 1st-order Roe scheme.
!!
subroutine FluxRoe1

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  use ModInterfaces, only : FluxWalls
  implicit none

! local variables
  integer     :: i, j, ie
  real(rtype) :: rhl, rhr, qsl, qsr, pav
  real(rtype) :: fc(4)

! *****************************************************************************
! initialize residual by adding artificial dissipation

  do i=1,nnodes
    rhs(1,i) = -diss(1,i)
    rhs(2,i) = -diss(2,i)
    rhs(3,i) = -diss(3,i)
    rhs(4,i) = -diss(4,i)
  enddo

! average of fluxes

  do ie=1,nedges
    i = edge(1,ie)
    j = edge(2,ie)

! - left and right rho*H, V*n

    rhl = dv(1,i) + cv(4,i)
    qsl = (cv(2,i)*sij(1,ie)+cv(3,i)*sij(2,ie))/cv(1,i)

    rhr = dv(1,j) + cv(4,j)
    qsr = (cv(2,j)*sij(1,ie)+cv(3,j)*sij(2,ie))/cv(1,j)

! - fluxes

    pav   = 0.5D0*(dv(1,i)+dv(1,j))
    fc(1) = 0.5D0*(qsl*cv(1,i)+qsr*cv(1,j))
    fc(2) = 0.5D0*(qsl*cv(2,i)+qsr*cv(2,j)) + sij(1,ie)*pav
    fc(3) = 0.5D0*(qsl*cv(3,i)+qsr*cv(3,j)) + sij(2,ie)*pav
    fc(4) = 0.5D0*(qsl*rhl    +qsr*rhr    )

    rhs(1,i) = rhs(1,i) + fc(1)
    rhs(2,i) = rhs(2,i) + fc(2)
    rhs(3,i) = rhs(3,i) + fc(3)
    rhs(4,i) = rhs(4,i) + fc(4)

    rhs(1,j) = rhs(1,j) - fc(1)
    rhs(2,j) = rhs(2,j) - fc(2)
    rhs(3,j) = rhs(3,j) - fc(3)
    rhs(4,j) = rhs(4,j) - fc(4)
  enddo

! treatment of solid walls

  call FluxWalls

end subroutine FluxRoe1

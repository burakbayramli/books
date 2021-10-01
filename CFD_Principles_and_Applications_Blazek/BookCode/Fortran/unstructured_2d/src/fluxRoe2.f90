!> @file fluxRoe2.f90
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
!! faces. The left and right fluxes are computed from reconstructed and limited
!! values at nodes "i" and "j". This is required for a 2nd-order Roe scheme
!! (see Subsection 4.3.3).
!!
subroutine FluxRoe2

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  use ModInterfaces, only : FluxWalls
  implicit none

! local variables
  integer     :: i, j, ie
  real(rtype) :: rx, ry, rrho, gam1, ggm1, rl, ul, vl, pl, hl, rr, &
                 ur, vr, pr, hr, qsrl, qsrr, pav
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
    i  = edge(1,ie)
    j  = edge(2,ie)
    rx = 0.5D0*(x(j)-x(i))
    ry = 0.5D0*(y(j)-y(i))

! - left & right state

    rrho = 1.D0/cv(1,i)
    gam1 = dv(4,i) - 1.D0
    ggm1 = dv(4,i)/gam1
    rl   = cv(1,i)      + lim(1,i)*(gradx(1,i)*rx+grady(1,i)*ry)
    ul   = cv(2,i)*rrho + lim(2,i)*(gradx(2,i)*rx+grady(2,i)*ry)
    vl   = cv(3,i)*rrho + lim(3,i)*(gradx(3,i)*rx+grady(3,i)*ry)
    pl   = dv(1,i)      + lim(4,i)*(gradx(4,i)*rx+grady(4,i)*ry)
    hl   = ggm1*pl/rl + 0.5D0*(ul*ul+vl*vl)
    qsrl = (ul*sij(1,ie)+vl*sij(2,ie))*rl

    rrho = 1.D0/cv(1,j)
    gam1 = dv(4,j) - 1.D0
    ggm1 = dv(4,j)/gam1
    rr   = cv(1,j)      - lim(1,j)*(gradx(1,j)*rx+grady(1,j)*ry)
    ur   = cv(2,j)*rrho - lim(2,j)*(gradx(2,j)*rx+grady(2,j)*ry)
    vr   = cv(3,j)*rrho - lim(3,j)*(gradx(3,j)*rx+grady(3,j)*ry)
    pr   = dv(1,j)      - lim(4,j)*(gradx(4,j)*rx+grady(4,j)*ry)
    hr   = ggm1*pr/rr + 0.5D0*(ur*ur+vr*vr)
    qsrr = (ur*sij(1,ie)+vr*sij(2,ie))*rr

! - fluxes

    pav   = 0.5D0*(pl+pr)
    fc(1) = 0.5D0*(qsrl   +qsrr   )
    fc(2) = 0.5D0*(qsrl*ul+qsrr*ur) + sij(1,ie)*pav
    fc(3) = 0.5D0*(qsrl*vl+qsrr*vr) + sij(2,ie)*pav
    fc(4) = 0.5D0*(qsrl*hl+qsrr*hr)

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

end subroutine FluxRoe2

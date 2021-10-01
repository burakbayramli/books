!> @file dissipRoe1.f90
!!
!! Computation of 1st-order upwind dissipation.
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

!> Computes upwind dissipation according to 1st-order Roe's flux-difference
!! splitting scheme.
!!
!! @param beta  coefficient for mixing new and old dissipation values
!!
subroutine DissipRoe1( beta )

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  implicit none

! parameters
  real(rtype), intent(in) :: beta

! local variables
  integer     :: i, j, ie
  real(rtype) :: beta5, ds, nx, ny, gam1, rrho, rl, ul, vl, pl, hl, &
                 rr, ur, vr, pr, hr, rav, dd, dd1, uav, vav, hav, q2a, &
                 c2a, cav, uv, du, h1, h2, h3, h4, h5, delta, eabs1, &
                 eabs2, eabs4
  real(rtype) :: fd(4)

! *****************************************************************************

  beta5 = 0.5D0*beta

  do ie=1,nedges
    i  = edge(1,ie)
    j  = edge(2,ie)
    ds = Sqrt(sij(1,ie)*sij(1,ie)+sij(2,ie)*sij(2,ie))
    nx = sij(1,ie)/ds
    ny = sij(2,ie)/ds

! - left & right state

    rrho = 1.D0/cv(1,i)
    rl   = cv(1,i)
    ul   = cv(2,i)*rrho
    vl   = cv(3,i)*rrho
    pl   = dv(1,i)
    hl   = (pl+cv(4,i))*rrho

    rrho = 1.D0/cv(1,j)
    rr   = cv(1,j)
    ur   = cv(2,j)*rrho
    vr   = cv(3,j)*rrho
    pr   = dv(1,j)
    hr   = (pr+cv(4,j))*rrho

! - Roe's average

    rav   = Sqrt(rl*rr)
    gam1  = 0.5D0*(dv(4,i)+dv(4,j)) - 1.D0
    dd    = rav/rl
    dd1   = 1.D0/(1.D0+dd)
    uav   = (ul+dd*ur)*dd1
    vav   = (vl+dd*vr)*dd1
    hav   = (hl+dd*hr)*dd1
    q2a   = 0.5D0*(uav*uav+vav*vav)
    c2a   = gam1*(hav-q2a)
    cav   = Sqrt(c2a)
    uv    = uav*nx + vav*ny
    du    = (ur-ul)*nx + (vr-vl)*ny

! - eigenvalues

    h1    = Abs(uv - cav)
    h2    = Abs(uv)
    h4    = Abs(uv + cav)
    delta = epsentr*h4

    eabs1 = EntropyCorr1( h1,delta )
    eabs2 = EntropyCorr1( h2,delta )
    eabs4 = EntropyCorr1( h4,delta )

! - upwind fluxes

    h1 = rav*cav*du
    h2 = eabs1*(pr-pl - h1)/(2.D0*c2a)
    h3 = eabs2*(rr-rl - (pr-pl)/c2a)
    h4 = eabs2*rav
    h5 = eabs4*(pr-pl + h1)/(2.D0*c2a)

    fd(1) = h2 + h3 + h5
    fd(2) = h2*(uav-cav*nx) + h3*uav + h4*(ur-ul-du*nx) + h5*(uav+cav*nx)
    fd(3) = h2*(vav-cav*ny) + h3*vav + h4*(vr-vl-du*ny) + h5*(vav+cav*ny)
    fd(4) = h2*(hav-cav*uv) + h3*q2a + h4*(uav*(ur-ul)+vav*(vr-vl)-uv*du) + &
            h5*(hav+cav*uv)

! - edge contributions to dissipation

    ds        = ds*beta5
    diss(1,i) = diss(1,i) + fd(1)*ds
    diss(2,i) = diss(2,i) + fd(2)*ds
    diss(3,i) = diss(3,i) + fd(3)*ds
    diss(4,i) = diss(4,i) + fd(4)*ds

    diss(1,j) = diss(1,j) - fd(1)*ds
    diss(2,j) = diss(2,j) - fd(2)*ds
    diss(3,j) = diss(3,j) - fd(3)*ds
    diss(4,j) = diss(4,j) - fd(4)*ds
  enddo

! *****************************************************************************

contains

!> Evaluates entropy correction function
!!
!! @param z  value to be corrected
!! @param d  threshold value of the correction
!! @return   corrected value of z
!!
  real(rtype) function EntropyCorr1( z,d )
    implicit none
    real(rtype) :: z, d

    if (z > d) then
      EntropyCorr1 = z
    else
      EntropyCorr1 = 0.5D0*(z*z+d*d)/d
    endif
  end function EntropyCorr1

end subroutine DissipRoe1

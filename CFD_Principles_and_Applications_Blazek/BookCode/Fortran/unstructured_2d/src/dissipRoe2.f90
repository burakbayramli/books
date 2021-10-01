!> @file dissipRoe2.f90
!!
!! Computation of 2nd-order upwind dissipation.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: May 30, 2014
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

!> Computes upwind dissipation according to 2nd-order Roe's flux-difference
!! splitting scheme. Values are extrapolated to the dual faces using Venkat's
!! limiter function.
!!
!! @param beta  coefficient for mixing new and old dissipation values
!!
subroutine DissipRoe2( beta )

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  implicit none

! parameters
  real(rtype), intent(in) :: beta

! local variables
  integer     :: i, j, ie
  real(rtype) :: beta5, ds, nx, ny, rx, ry, gam1, ggm1, rrho, rl, ul, vl, &
                 pl, hl, rr, ur, vr, pr, hr, rav, dd, dd1, uav, vav, hav, &
                 q2a, c2a, cav, uv, du, h1, h2, h3, h4, h5, delta, eabs1, &
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

    rrho = 1.D0/cv(1,j)
    gam1 = dv(4,j) - 1.D0
    ggm1 = dv(4,j)/gam1
    rr   = cv(1,j)      - lim(1,j)*(gradx(1,j)*rx+grady(1,j)*ry)
    ur   = cv(2,j)*rrho - lim(2,j)*(gradx(2,j)*rx+grady(2,j)*ry)
    vr   = cv(3,j)*rrho - lim(3,j)*(gradx(3,j)*rx+grady(3,j)*ry)
    pr   = dv(1,j)      - lim(4,j)*(gradx(4,j)*rx+grady(4,j)*ry)
    hr   = ggm1*pr/rr + 0.5D0*(ur*ur+vr*vr)

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

    eabs1 = EntropyCorr2( h1,delta )
    eabs2 = EntropyCorr2( h2,delta )
    eabs4 = EntropyCorr2( h4,delta )

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
  real(rtype) function EntropyCorr2( z,d )
    implicit none
    real(rtype) :: z, d

    if (z > d) then
      EntropyCorr2 = z
    else
      EntropyCorr2 = 0.5D0*(z*z+d*d)/d
    endif
  end function EntropyCorr2

end subroutine DissipRoe2

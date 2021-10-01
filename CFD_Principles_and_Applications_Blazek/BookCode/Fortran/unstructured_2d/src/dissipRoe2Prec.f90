!> @file dissipRoe2Prec.f90
!!
!! Computation of 2nd-order upwind dissipation with preconditioning.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: September 19, 2014
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
!! limiter function. The dissipation terms are multiplied by a preconditioning
!! matrix for low Mach numbers (see Eqs. (9.54), (9.82)).
!!
!! @param beta  coefficient for mixing new and old dissipation values
!!
subroutine DissipRoe2Prec( beta )

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  use ModInterfaces, only : CompTheta, RightEigenvec, MatprodTp1_P1, MatVecProd5
  implicit none

! parameters
  real(rtype), intent(in) :: beta

! local variables
  integer     :: i, j, ie
  real(rtype) :: beta5, ds, rx, ry, gam1, ggm1, rgas, rrho, rl, ul, vl, pl, &
                 tl, hl, rr, ur, vr, pr, tr, hr, rav, dd, dd1, uav, vav, hav, &
                 pav, tav, cav, q2a, uv, h1, h2, h5, delta, eabs1, eabs2, &
                 eabs5, a1, a4, a5, cc2, ra1g, rhop, rhoT, hT, theta
  real(rtype) :: fd(5), nVec(3), gtpMat(5,5), tp1Mat(5,5)
  real(rtype) :: wVec(5), wpVec(5), wrlVec(5), dumVec(5)

! *****************************************************************************

  beta5 = 0.5D0*beta

  nVec(3)   = 0.D0   ! no 3rd dimension here
  wrlVec(4) = 0.D0
  wVec(4)   = 0.D0
  wpVec(4)  = 0.D0

  do ie=1,nedges
    i       = edge(1,ie)
    j       = edge(2,ie)
    ds      = Sqrt(sij(1,ie)*sij(1,ie)+sij(2,ie)*sij(2,ie))
    nVec(1) = sij(1,ie)/ds
    nVec(2) = sij(2,ie)/ds
    rx      = 0.5D0*(x(j)-x(i))
    ry      = 0.5D0*(y(j)-y(i))

! - left & right state

    rrho = 1.D0/cv(1,i)
    gam1 = dv(4,i) - 1.D0
    ggm1 = dv(4,i)/gam1
    rgas = gam1*dv(5,i)/dv(4,i)
    rl   = cv(1,i)      + lim(1,i)*(gradx(1,i)*rx+grady(1,i)*ry)
    ul   = cv(2,i)*rrho + lim(2,i)*(gradx(2,i)*rx+grady(2,i)*ry)
    vl   = cv(3,i)*rrho + lim(3,i)*(gradx(3,i)*rx+grady(3,i)*ry)
    pl   = dv(1,i)      + lim(4,i)*(gradx(4,i)*rx+grady(4,i)*ry)
    tl   = pl/(rgas*rl)
    hl   = ggm1*pl/rl + 0.5D0*(ul*ul+vl*vl)

    rrho = 1.D0/cv(1,j)
    gam1 = dv(4,j) - 1.D0
    ggm1 = dv(4,j)/gam1
    rgas = gam1*dv(5,j)/dv(4,j)
    rr   = cv(1,j)      - lim(1,j)*(gradx(1,j)*rx+grady(1,j)*ry)
    ur   = cv(2,j)*rrho - lim(2,j)*(gradx(2,j)*rx+grady(2,j)*ry)
    vr   = cv(3,j)*rrho - lim(3,j)*(gradx(3,j)*rx+grady(3,j)*ry)
    pr   = dv(1,j)      - lim(4,j)*(gradx(4,j)*rx+grady(4,j)*ry)
    tr   = pr/(rgas*rr)
    hr   = ggm1*pr/rr + 0.5D0*(ur*ur+vr*vr)

! - Roe's average

    rav  = Sqrt(rl*rr)
    gam1 = 0.5D0*(dv(4,i)+dv(4,j)) - 1.D0
    dd   = rav/rl
    dd1  = 1.D0/(1.D0+dd)
    uav  = (ul+dd*ur)*dd1
    vav  = (vl+dd*vr)*dd1
    pav  = (pl+dd*pr)*dd1
    tav  = (tl+dd*tr)*dd1
    hav  = (hl+dd*hr)*dd1
    q2a  = uav*uav + vav*vav
    cav  = Sqrt(gam1*(hav-0.5D0*q2a))
    uv   = uav*nVec(1) + vav*nVec(2)

! - preconditioning

    wrlVec(1) = rr - rl
    wrlVec(2) = rr*ur - rl*ul
    wrlVec(3) = rr*vr - rl*vl
    wrlVec(5) = (rr*hr-pr) - (rl*hl-pl)

    wVec(1) = rav
    wVec(2) = rav*uav
    wVec(3) = rav*vav
    wVec(5) = rav*hav - pav

    wpVec(1) = pav
    wpVec(2) = uav
    wpVec(3) = vav
    wpVec(5) = tav

    rhop  =  rav/pav
    rhoT  = -rav/tav
    hT    = 0.5D0*(dv(5,i)+dv(5,j))
    theta = CompTheta( gam1+1.D0,cav,q2a )

    delta = epsentr*cav
    h2    = Abs(uv)
    eabs2 = EntropyCorr2Prec( h2,delta )
    a1    = wVec(1)*rhop*hT + rhoT
    ra1g  = 1.D0/(wVec(1)*theta*hT + rhoT)
    a4    = a1*ra1g
    a5    = wVec(1)*hT*ra1g
    cc2   = 0.5D0*Sqrt((uv*uv)*((a4-1.D0)*(a4-1.D0))+4.D0*a5)
    h2    = 0.5D0*(a4+1.D0)*uv
    h1    = Abs(h2 + cc2)
    h5    = Abs(h2 - cc2)
    eabs1 = EntropyCorr2Prec( h1,delta )
    eabs5 = EntropyCorr2Prec( h5,delta )

    call RightEigenvec( wVec,wpVec,nVec,uv,hav,theta,rhop,rhoT,0.D0,hT,gtpMat )
    call MatprodTp1_P1( wVec,wpVec,nVec,uv,hav,theta,rhop,rhoT,0.D0,hT,q2a,tp1Mat )

! - upwind fluxes

    call MatVecProd5( tp1Mat,wrlVec,dumVec )
    dumVec(1) = dumVec(1)*eabs2
    dumVec(2) = dumVec(2)*eabs2
    dumVec(3) = dumVec(3)*eabs2
    dumVec(4) = dumVec(4)*eabs1
    dumVec(5) = dumVec(5)*eabs5
    call MatVecProd5( gtpMat,dumVec,fd )

! - edge contributions to dissipation

    ds        = ds*beta5
    diss(1,i) = diss(1,i) + fd(1)*ds
    diss(2,i) = diss(2,i) + fd(2)*ds
    diss(3,i) = diss(3,i) + fd(3)*ds
    diss(4,i) = diss(4,i) + fd(5)*ds

    diss(1,j) = diss(1,j) - fd(1)*ds
    diss(2,j) = diss(2,j) - fd(2)*ds
    diss(3,j) = diss(3,j) - fd(3)*ds
    diss(4,j) = diss(4,j) - fd(5)*ds
  enddo

! *****************************************************************************

contains

!> Evaluates entropy correction function
!!
!! @param z  value to be corrected
!! @param d  threshold value of the correction
!! @return   corrected value of z
!!
  real(rtype) function EntropyCorr2Prec( z,d )
    implicit none
    real(rtype) :: z, d

    if (z > d) then
      EntropyCorr2Prec = z
    else
      EntropyCorr2Prec = 0.5D0*(z*z+d*d)/d
    endif
  end function EntropyCorr2Prec

end subroutine DissipRoe2Prec

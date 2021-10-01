!> @file dissipRoe1Prec.f90
!!
!! Computation of 1st-order upwind dissipation with preconditioning.
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

!> Computes upwind dissipation according to 1st-order Roe's flux-difference
!! splitting scheme. The dissipation terms are multiplied by a preconditioning
!! matrix for low Mach numbers (see Eqs. (9.54), (9.82)).
!!
!! @param beta  coefficient for mixing new and old dissipation values
!!
subroutine DissipRoe1Prec( beta )

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
  real(rtype) :: beta5, ds, gam1, rrho, rl, ul, vl, pl, tl, hl, &
                 rr, ur, vr, pr, tr, hr, rav, dd, dd1, uav, vav, pav, tav, &
                 hav, cav, q2a, uv, h1, h2, h5, delta, eabs1, eabs2, eabs5, &
                 a1, a4, a5, cc2, ra1g, rhop, rhoT, hT, theta
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

! - left & right state

    rrho = 1.D0/cv(1,i)
    rl   = cv(1,i)
    ul   = cv(2,i)*rrho
    vl   = cv(3,i)*rrho
    pl   = dv(1,i)
    tl   = dv(2,i)
    hl   = (pl+cv(4,i))*rrho

    rrho = 1.D0/cv(1,j)
    rr   = cv(1,j)
    ur   = cv(2,j)*rrho
    vr   = cv(3,j)*rrho
    pr   = dv(1,j)
    tr   = dv(2,j)
    hr   = (pr+cv(4,j))*rrho

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
    wrlVec(2) = cv(2,j) - cv(2,i)
    wrlVec(3) = cv(3,j) - cv(3,i)
    wrlVec(5) = cv(4,j) - cv(4,i)

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
    eabs2 = EntropyCorr1Prec( h2,delta )
    a1    = wVec(1)*rhop*hT + rhoT
    ra1g  = 1.D0/(wVec(1)*theta*hT + rhoT)
    a4    = a1*ra1g
    a5    = wVec(1)*hT*ra1g
    cc2   = 0.5D0*Sqrt((uv*uv)*((a4-1.D0)*(a4-1.D0))+4.D0*a5)
    h2    = 0.5D0*(a4+1.D0)*uv
    h1    = Abs(h2 + cc2)
    h5    = Abs(h2 - cc2)
    eabs1 = EntropyCorr1Prec( h1,delta )
    eabs5 = EntropyCorr1Prec( h5,delta )

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
  real(rtype) function EntropyCorr1Prec( z,d )
    implicit none
    real(rtype) :: z, d

    if (z > d) then
      EntropyCorr1Prec = z
    else
      EntropyCorr1Prec = 0.5D0*(z*z+d*d)/d
    endif
  end function EntropyCorr1Prec

end subroutine DissipRoe1Prec

!> @file fluxViscous.f90
!!
!! Computation of the viscous fluxes.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: July 18, 2014
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

!> Computes viscous fluxes for the Navier-Stokes equations and adds
!! them to the dissipation variable (diss). Gradients at the faces
!! of the control volumes are obtained by a modified averaging of
!! node-based gradients (see Section 5.4.2).
!!
!! @param beta  coefficient for mixing new and old dissipation values
!!
subroutine FluxViscous( beta )

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  implicit none

! parameters
  real(rtype), intent(in) :: beta

! local variables
  integer     :: i, j, ie
  real(rtype) :: two3, ui, uj, vi, vj, uav, vav, mav, kav
  real(rtype) :: txn, tyn, ds2, rds, duxa, duya, dvxa, dvya, dtxa, dtya
  real(rtype) :: duds, dvds, dtds, dudt, dvdt, dtdt
  real(rtype) :: duxf, duyf, dvxf, dvyf, dtxf, dtyf
  real(rtype) :: tauxx, tauxy, tauyy, phix, phiy
  real(rtype) :: fv(3)

! *****************************************************************************

  two3 = 2.D0/3.D0

! interior edges --------------------------------------------------------------

  do ie=1,nedint
    i = edge(1,ie)
    j = edge(2,ie)

! - average of flow variables

    ui  = cv(2,i)/cv(1,i)
    uj  = cv(2,j)/cv(1,j)
    vi  = cv(3,i)/cv(1,i)
    vj  = cv(3,j)/cv(1,j)
    uav = 0.5D0*(ui+uj)
    vav = 0.5D0*(vi+vj)
    mav = 0.5D0*(dv(6,i)+dv(6,j))
    kav = 0.5D0*(dv(7,i)+dv(7,j))

! - tangential vector (normalized)

    txn = x(j) - x(i)
    tyn = y(j) - y(i)
    ds2 = txn*txn + tyn*tyn
    rds = 1.D0/Sqrt(ds2)
    txn = txn*rds
    tyn = tyn*rds

! - average of gradients

    duxa = 0.5D0*(gradx(2,i)+gradx(2,j))
    duya = 0.5D0*(grady(2,i)+grady(2,j))
    dvxa = 0.5D0*(gradx(3,i)+gradx(3,j))
    dvya = 0.5D0*(grady(3,i)+grady(3,j))
    dtxa = 0.5D0*(gradx(5,i)+gradx(5,j))
    dtya = 0.5D0*(grady(5,i)+grady(5,j))

! - divided difference

    duds = rds*(uj-ui)
    dvds = rds*(vj-vi)
    dtds = rds*(dv(2,j)-dv(2,i))

! - tangential component - divided difference

    dudt = duxa*txn + duya*tyn - duds
    dvdt = dvxa*txn + dvya*tyn - dvds
    dtdt = dtxa*txn + dtya*tyn - dtds

! - face gradients (Eq. (5.73))

    duxf = duxa - dudt*txn
    duyf = duya - dudt*tyn
    dvxf = dvxa - dvdt*txn
    dvyf = dvya - dvdt*tyn
    dtxf = dtxa - dtdt*txn
    dtyf = dtya - dtdt*tyn

! - viscous fluxes

    tauxx = two3*mav*(2.D0*duxf-dvyf)
    tauyy = two3*mav*(2.D0*dvyf-duxf)
    tauxy =      mav*(     duyf+dvxf)
    phix  = uav*tauxx + vav*tauxy + kav*dtxf
    phiy  = uav*tauxy + vav*tauyy + kav*dtyf

    fv(1) = sij(1,ie)*tauxx + sij(2,ie)*tauxy
    fv(2) = sij(1,ie)*tauxy + sij(2,ie)*tauyy
    fv(3) = sij(1,ie)*phix  + sij(2,ie)*phiy

! - edge contributions to dissipation

    diss(2,i) = diss(2,i) + fv(1)*beta
    diss(3,i) = diss(3,i) + fv(2)*beta
    diss(4,i) = diss(4,i) + fv(3)*beta

    diss(2,j) = diss(2,j) - fv(1)*beta
    diss(3,j) = diss(3,j) - fv(2)*beta
    diss(4,j) = diss(4,j) - fv(3)*beta
  enddo

! edges to dummy nodes --------------------------------------------------------

  do ie=nedint+1,nedges
    i = edge(1,ie)
    j = edge(2,ie)

! - average of variables

    uav = 0.5D0*(cv(2,i)/cv(1,i)+cv(2,j)/cv(1,j))
    vav = 0.5D0*(cv(3,i)/cv(1,i)+cv(3,j)/cv(1,j))
    mav = 0.5D0*(dv(6,i)+dv(6,j))
    kav = 0.5D0*(dv(7,i)+dv(7,j))

! - viscous fluxes

    tauxx = two3*mav*(2.D0*gradx(2,i)-grady(3,i))
    tauyy = two3*mav*(2.D0*grady(3,i)-gradx(2,i))
    tauxy =      mav*(     grady(2,i)+gradx(3,i))
    phix  = uav*tauxx + vav*tauxy + kav*gradx(5,i)
    phiy  = uav*tauxy + vav*tauyy + kav*grady(5,i)

    fv(1) = sij(1,ie)*tauxx + sij(2,ie)*tauxy
    fv(2) = sij(1,ie)*tauxy + sij(2,ie)*tauyy
    fv(3) = sij(1,ie)*phix  + sij(2,ie)*phiy

    diss(2,i) = diss(2,i) + fv(1)*beta
    diss(3,i) = diss(3,i) + fv(2)*beta
    diss(4,i) = diss(4,i) + fv(3)*beta
  enddo

end subroutine FluxViscous

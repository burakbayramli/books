!> @file fluxViscous.f90
!!
!! Computation of the viscous flux.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: April 2, 2014
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

!> Computes viscous flux for the Navier-Stokes equations and adds
!! it to the dissipation variable (diss).
!!
!! @param beta  coefficient for mixing new and old dissipation values
!! @param u     u-velocity (computed in GradsInitial)
!! @param v     v-velocity (computed in GradsInitial)
!!
subroutine FluxViscous( beta,u,v )

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  implicit none

! parameters
  real(rtype), intent(in) :: beta
  real(rtype) :: u(:,:), v(:,:)

! local variables
  integer     :: i, j, im1, jm1
  real(rtype) :: two3, uav, vav, mav, kav
  real(rtype) :: tauxx, tauxy, tauyy, phix, phiy
  real(rtype) :: fv(3)

! *****************************************************************************

  two3 = 2.D0/3.D0

! i-direction -----------------------------------------------------------------

  do j=2,j2
    do i=2,il
      im1   = i - 1
      uav   = 0.5D0*(   u(im1,j)+   u(i,j))
      vav   = 0.5D0*(   v(im1,j)+   v(i,j))
      mav   = 0.5D0*(dv(6,im1,j)+dv(6,i,j))
      kav   = 0.5D0*(dv(7,im1,j)+dv(7,i,j))
      tauxx = two3*mav*(2.D0*gradfi(1,i,j)-gradfi(4,i,j))
      tauyy = two3*mav*(2.D0*gradfi(4,i,j)-gradfi(1,i,j))
      tauxy =      mav*(     gradfi(2,i,j)+gradfi(3,i,j))
      phix  = uav*tauxx + vav*tauxy + kav*gradfi(5,i,j)
      phiy  = uav*tauxy + vav*tauyy + kav*gradfi(6,i,j)
      fv(1) = si(1,i,j)*tauxx + si(2,i,j)*tauxy
      fv(2) = si(1,i,j)*tauxy + si(2,i,j)*tauyy
      fv(3) = si(1,i,j)*phix  + si(2,i,j)*phiy

! --- dissipation term

      diss(2,i  ,j) = diss(2,i  ,j) + beta*fv(1)
      diss(3,i  ,j) = diss(3,i  ,j) + beta*fv(2)
      diss(4,i  ,j) = diss(4,i  ,j) + beta*fv(3)

      diss(2,im1,j) = diss(2,im1,j) - beta*fv(1)
      diss(3,im1,j) = diss(3,im1,j) - beta*fv(2)
      diss(4,im1,j) = diss(4,im1,j) - beta*fv(3)
    enddo
  enddo

! j-direction -----------------------------------------------------------------

  do i=2,i2
    do j=2,jl
      jm1   = j - 1
      uav   = 0.5D0*(   u(i,jm1)+   u(i,j))
      vav   = 0.5D0*(   v(i,jm1)+   v(i,j))
      mav   = 0.5D0*(dv(6,i,jm1)+dv(6,i,j))
      kav   = 0.5D0*(dv(7,i,jm1)+dv(7,i,j))
      tauxx = two3*mav*(2.D0*gradfj(1,i,j)-gradfj(4,i,j))
      tauyy = two3*mav*(2.D0*gradfj(4,i,j)-gradfj(1,i,j))
      tauxy =      mav*(     gradfj(2,i,j)+gradfj(3,i,j))
      phix  = uav*tauxx + vav*tauxy + kav*gradfj(5,i,j)
      phiy  = uav*tauxy + vav*tauyy + kav*gradfj(6,i,j)
      fv(1) = sj(1,i,j)*tauxx + sj(2,i,j)*tauxy
      fv(2) = sj(1,i,j)*tauxy + sj(2,i,j)*tauyy
      fv(3) = sj(1,i,j)*phix  + sj(2,i,j)*phiy

! --- dissipation term

      diss(2,i,j  ) = diss(2,i,j  ) + beta*fv(1)
      diss(3,i,j  ) = diss(3,i,j  ) + beta*fv(2)
      diss(4,i,j  ) = diss(4,i,j  ) + beta*fv(3)

      diss(2,i,jm1) = diss(2,i,jm1) - beta*fv(1)
      diss(3,i,jm1) = diss(3,i,jm1) - beta*fv(2)
      diss(4,i,jm1) = diss(4,i,jm1) - beta*fv(3)
    enddo
  enddo

end subroutine FluxViscous

!> @file dissipCentral.f90
!!
!! Computation of central artificial dissipation.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: March 15, 2014
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

!> Computes artificial dissipation as a blend of 2nd- and 4th-order central
!! differences. This corresponds to the JST scheme.
!!
!! @param beta  coefficient for mixing new and old dissipation values
!! @param dp    temporary storage for the pressure sensor
!!
subroutine DissipCentral( beta,dp )

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  implicit none

! parameters
  real(rtype), intent(in) :: beta
  real(rtype) :: dp(0:)

! local variables
  integer     :: i, j, im1, ip1, ip2, jm1, jp1, jp2
  real(rtype) :: eval, pmax, eps2, eps4
  real(rtype) :: fd(4)

! *****************************************************************************
! i-direction -----------------------------------------------------------------

  do j=2,j2

! - pressure sensor (divided second differences)

    do i=1,il
      dp(i) = Abs((dv(1,i+1,j)-2.D0*dv(1,i,j)+dv(1,i-1,j))/ &
                  (dv(1,i+1,j)+2.D0*dv(1,i,j)+dv(1,i-1,j)))
    enddo

! - dissipation fluxes (at I+1/2)

    do i=1,i2
      im1   = i - 1
      ip1   = i + 1
      ip2   = i + 2
      eval  = 0.5D0*(sri(i,j)+sri(ip1,j)+srj(i,j)+srj(ip1,j))
      pmax  = Max(dp(i),dp(ip1))
      eps2  = eval*vis2*pmax
      eps4  = eval*vis4
      eps4  = Dim(eps4,eps2)
      fd(1) = eps2*(cv(1,ip1,j)-cv(1,i,j)) - &
              eps4*(cv(1,ip2,j)-3.D0*(cv(1,ip1,j)-cv(1,i,j))-cv(1,im1,j))
      fd(2) = eps2*(cv(2,ip1,j)-cv(2,i,j)) - &
              eps4*(cv(2,ip2,j)-3.D0*(cv(2,ip1,j)-cv(2,i,j))-cv(2,im1,j))
      fd(3) = eps2*(cv(3,ip1,j)-cv(3,i,j)) - &
              eps4*(cv(3,ip2,j)-3.D0*(cv(3,ip1,j)-cv(3,i,j))-cv(3,im1,j))
      fd(4) = eps2*(cv(4,ip1,j)-cv(4,i,j)) - &
              eps4*(cv(4,ip2,j)-3.D0*(cv(4,ip1,j)-cv(4,i,j))-cv(4,im1,j))

! --- dissipation term

      diss(1,i  ,j) = diss(1,i  ,j) + beta*fd(1)
      diss(2,i  ,j) = diss(2,i  ,j) + beta*fd(2)
      diss(3,i  ,j) = diss(3,i  ,j) + beta*fd(3)
      diss(4,i  ,j) = diss(4,i  ,j) + beta*fd(4)

      diss(1,ip1,j) = diss(1,ip1,j) - beta*fd(1)
      diss(2,ip1,j) = diss(2,ip1,j) - beta*fd(2)
      diss(3,ip1,j) = diss(3,ip1,j) - beta*fd(3)
      diss(4,ip1,j) = diss(4,ip1,j) - beta*fd(4)
    enddo

  enddo

! j-direction -----------------------------------------------------------------

  do i=2,i2

! - pressure sensor (divided second differences)

    do j=1,jl
      dp(j) = Abs((dv(1,i,j+1)-2.D0*dv(1,i,j)+dv(1,i,j-1))/ &
                  (dv(1,i,j+1)+2.D0*dv(1,i,j)+dv(1,i,j-1)))
    enddo

! - dissipation fluxes (at J+1/2)

    do j=1,j2
      jm1   = j - 1
      jp1   = j + 1
      jp2   = j + 2
      eval  = 0.5D0*(sri(i,j)+sri(i,jp1)+srj(i,j)+srj(i,jp1))
      pmax  = Max(dp(j),dp(jp1))
      eps2  = eval*vis2*pmax
      eps4  = eval*vis4
      eps4  = Dim(eps4,eps2)
      fd(1) = eps2*(cv(1,i,jp1)-cv(1,i,j)) - &
              eps4*(cv(1,i,jp2)-3.D0*(cv(1,i,jp1)-cv(1,i,j))-cv(1,i,jm1))
      fd(2) = eps2*(cv(2,i,jp1)-cv(2,i,j)) - &
              eps4*(cv(2,i,jp2)-3.D0*(cv(2,i,jp1)-cv(2,i,j))-cv(2,i,jm1))
      fd(3) = eps2*(cv(3,i,jp1)-cv(3,i,j)) - &
              eps4*(cv(3,i,jp2)-3.D0*(cv(3,i,jp1)-cv(3,i,j))-cv(3,i,jm1))
      fd(4) = eps2*(cv(4,i,jp1)-cv(4,i,j)) - &
              eps4*(cv(4,i,jp2)-3.D0*(cv(4,i,jp1)-cv(4,i,j))-cv(4,i,jm1))

! --- dissipation term

      diss(1,i,j  ) = diss(1,i,j  ) + beta*fd(1)
      diss(2,i,j  ) = diss(2,i,j  ) + beta*fd(2)
      diss(3,i,j  ) = diss(3,i,j  ) + beta*fd(3)
      diss(4,i,j  ) = diss(4,i,j  ) + beta*fd(4)

      diss(1,i,jp1) = diss(1,i,jp1) - beta*fd(1)
      diss(2,i,jp1) = diss(2,i,jp1) - beta*fd(2)
      diss(3,i,jp1) = diss(3,i,jp1) - beta*fd(3)
      diss(4,i,jp1) = diss(4,i,jp1) - beta*fd(4)
    enddo

  enddo

end subroutine DissipCentral

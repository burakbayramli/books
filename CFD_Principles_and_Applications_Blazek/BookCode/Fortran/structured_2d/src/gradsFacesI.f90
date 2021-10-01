!> @file gradsFacesI.f90
!!
!! Computation of gradients at i-faces.
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

!> Computes gradients of u, v, and T with respect to the x- and y-coordinates
!! at the faces "i" of the grid cells.
!!
!! @param u  u-velocity (computed in GradsInitial)
!! @param v  v-velocity (computed in GradsInitial)
!!
subroutine GradsFacesI( u,v )

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  implicit none

! parameters
  real(rtype) :: u(:,:), v(:,:)

! local variables
  integer     :: i, j
  real(rtype) :: sx, sy, uav, vav, tav, rvol
  real(rtype) :: fgx(3), fgy(3)

! *****************************************************************************
! sum up contributions to gradients

  do j=2,jl
    do i=2,i2

! --- right face of auxiliary control volume

      sx     = 0.5D0*(si(1,i,j)+si(1,i+1,j))
      sy     = 0.5D0*(si(2,i,j)+si(2,i+1,j))
      fgx(1) =    u(i,j)*sx
      fgx(2) =    v(i,j)*sx
      fgx(3) = dv(2,i,j)*sx
      fgy(1) =    u(i,j)*sy
      fgy(2) =    v(i,j)*sy
      fgy(3) = dv(2,i,j)*sy

      gradfi(1,i  ,j) = gradfi(1,i  ,j) - fgx(1)
      gradfi(2,i  ,j) = gradfi(2,i  ,j) - fgy(1)
      gradfi(3,i  ,j) = gradfi(3,i  ,j) - fgx(2)
      gradfi(4,i  ,j) = gradfi(4,i  ,j) - fgy(2)
      gradfi(5,i  ,j) = gradfi(5,i  ,j) - fgx(3)
      gradfi(6,i  ,j) = gradfi(6,i  ,j) - fgy(3)

      gradfi(1,i+1,j) = gradfi(1,i+1,j) + fgx(1)
      gradfi(2,i+1,j) = gradfi(2,i+1,j) + fgy(1)
      gradfi(3,i+1,j) = gradfi(3,i+1,j) + fgx(2)
      gradfi(4,i+1,j) = gradfi(4,i+1,j) + fgy(2)
      gradfi(5,i+1,j) = gradfi(5,i+1,j) + fgx(3)
      gradfi(6,i+1,j) = gradfi(6,i+1,j) + fgy(3)

! --- bottom face of auxiliary control volume

      sx     = 0.5D0*(sj(1,i,j)+sj(1,i-1,j))
      sy     = 0.5D0*(sj(2,i,j)+sj(2,i-1,j))
      uav    = 0.25D0*(u(i,j)+u(i-1,j)+u(i,j-1)+u(i-1,j-1))
      vav    = 0.25D0*(v(i,j)+v(i-1,j)+v(i,j-1)+v(i-1,j-1))
      tav    = 0.25D0*(dv(2,i,j)+dv(2,i-1,j)+dv(2,i,j-1)+dv(2,i-1,j-1))
      fgx(1) = uav*sx
      fgx(2) = vav*sx
      fgx(3) = tav*sx
      fgy(1) = uav*sy
      fgy(2) = vav*sy
      fgy(3) = tav*sy

      gradfi(1,i,j  ) = gradfi(1,i,j  ) + fgx(1)
      gradfi(2,i,j  ) = gradfi(2,i,j  ) + fgy(1)
      gradfi(3,i,j  ) = gradfi(3,i,j  ) + fgx(2)
      gradfi(4,i,j  ) = gradfi(4,i,j  ) + fgy(2)
      gradfi(5,i,j  ) = gradfi(5,i,j  ) + fgx(3)
      gradfi(6,i,j  ) = gradfi(6,i,j  ) + fgy(3)

      gradfi(1,i,j-1) = gradfi(1,i,j-1) - fgx(1)
      gradfi(2,i,j-1) = gradfi(2,i,j-1) - fgy(1)
      gradfi(3,i,j-1) = gradfi(3,i,j-1) - fgx(2)
      gradfi(4,i,j-1) = gradfi(4,i,j-1) - fgy(2)
      gradfi(5,i,j-1) = gradfi(5,i,j-1) - fgx(3)
      gradfi(6,i,j-1) = gradfi(6,i,j-1) - fgy(3)
    enddo ! i

! - treat boundary i=2

    sx     = si(1,2,j)
    sy     = si(2,2,j)
    fgx(1) = 0.5D0*(   u(1,j)+   u(2,j))*sx
    fgx(2) = 0.5D0*(   v(1,j)+   v(2,j))*sx
    fgx(3) = 0.5D0*(dv(2,1,j)+dv(2,2,j))*sx
    fgy(1) = 0.5D0*(   u(1,j)+   u(2,j))*sy
    fgy(2) = 0.5D0*(   v(1,j)+   v(2,j))*sy
    fgy(3) = 0.5D0*(dv(2,1,j)+dv(2,2,j))*sy

    gradfi(1,2,j) = gradfi(1,2,j) + fgx(1)
    gradfi(2,2,j) = gradfi(2,2,j) + fgy(1)
    gradfi(3,2,j) = gradfi(3,2,j) + fgx(2)
    gradfi(4,2,j) = gradfi(4,2,j) + fgy(2)
    gradfi(5,2,j) = gradfi(5,2,j) + fgx(3)
    gradfi(6,2,j) = gradfi(6,2,j) + fgy(3)

! - treat boundary i=il

    sx     = si(1,il,j)
    sy     = si(2,il,j)
    fgx(1) = 0.5D0*(   u(i2,j)+   u(il,j))*sx
    fgx(2) = 0.5D0*(   v(i2,j)+   v(il,j))*sx
    fgx(3) = 0.5D0*(dv(2,i2,j)+dv(2,il,j))*sx
    fgy(1) = 0.5D0*(   u(i2,j)+   u(il,j))*sy
    fgy(2) = 0.5D0*(   v(i2,j)+   v(il,j))*sy
    fgy(3) = 0.5D0*(dv(2,i2,j)+dv(2,il,j))*sy

    gradfi(1,il,j) = gradfi(1,il,j) - fgx(1)
    gradfi(2,il,j) = gradfi(2,il,j) - fgy(1)
    gradfi(3,il,j) = gradfi(3,il,j) - fgx(2)
    gradfi(4,il,j) = gradfi(4,il,j) - fgy(2)
    gradfi(5,il,j) = gradfi(5,il,j) - fgx(3)
    gradfi(6,il,j) = gradfi(6,il,j) - fgy(3)

    sx     = 0.5D0*sj(1,i2,j)
    sy     = 0.5D0*sj(2,i2,j)
    uav    = 0.25D0*(u(i2,j)+u(il,j)+u(i2,j-1)+u(il,j-1))
    vav    = 0.25D0*(v(i2,j)+v(il,j)+v(i2,j-1)+v(il,j-1))
    tav    = 0.25D0*(dv(2,i2,j)+dv(2,il,j)+dv(2,i2,j-1)+dv(2,il,j-1))
    fgx(1) = uav*sx
    fgx(2) = vav*sx
    fgx(3) = tav*sx
    fgy(1) = uav*sy
    fgy(2) = vav*sy
    fgy(3) = tav*sy

    gradfi(1,il,j  ) = gradfi(1,il,j  ) + fgx(1)
    gradfi(2,il,j  ) = gradfi(2,il,j  ) + fgy(1)
    gradfi(3,il,j  ) = gradfi(3,il,j  ) + fgx(2)
    gradfi(4,il,j  ) = gradfi(4,il,j  ) + fgy(2)
    gradfi(5,il,j  ) = gradfi(5,il,j  ) + fgx(3)
    gradfi(6,il,j  ) = gradfi(6,il,j  ) + fgy(3)

    gradfi(1,il,j-1) = gradfi(1,il,j-1) - fgx(1)
    gradfi(2,il,j-1) = gradfi(2,il,j-1) - fgy(1)
    gradfi(3,il,j-1) = gradfi(3,il,j-1) - fgx(2)
    gradfi(4,il,j-1) = gradfi(4,il,j-1) - fgy(2)
    gradfi(5,il,j-1) = gradfi(5,il,j-1) - fgx(3)
    gradfi(6,il,j-1) = gradfi(6,il,j-1) - fgy(3)
  enddo ! j

! divide by the volume of auxiliary cell

  do j=2,j2
    do i=3,i2
      rvol          = 2.D0/(vol(i,j)+vol(i-1,j))
      gradfi(1,i,j) = gradfi(1,i,j)*rvol
      gradfi(2,i,j) = gradfi(2,i,j)*rvol
      gradfi(3,i,j) = gradfi(3,i,j)*rvol
      gradfi(4,i,j) = gradfi(4,i,j)*rvol
      gradfi(5,i,j) = gradfi(5,i,j)*rvol
      gradfi(6,i,j) = gradfi(6,i,j)*rvol
    enddo

    rvol           = 2.D0/vol(2 ,j)
    gradfi(1,2 ,j) = gradfi(1,2 ,j)*rvol
    gradfi(2,2 ,j) = gradfi(2,2 ,j)*rvol
    gradfi(3,2 ,j) = gradfi(3,2 ,j)*rvol
    gradfi(4,2 ,j) = gradfi(4,2 ,j)*rvol
    gradfi(5,2 ,j) = gradfi(5,2 ,j)*rvol
    gradfi(6,2 ,j) = gradfi(6,2 ,j)*rvol

    rvol           = 2.D0/vol(i2,j)
    gradfi(1,il,j) = gradfi(1,il,j)*rvol
    gradfi(2,il,j) = gradfi(2,il,j)*rvol
    gradfi(3,il,j) = gradfi(3,il,j)*rvol
    gradfi(4,il,j) = gradfi(4,il,j)*rvol
    gradfi(5,il,j) = gradfi(5,il,j)*rvol
    gradfi(6,il,j) = gradfi(6,il,j)*rvol
  enddo ! j

end subroutine GradsFacesI

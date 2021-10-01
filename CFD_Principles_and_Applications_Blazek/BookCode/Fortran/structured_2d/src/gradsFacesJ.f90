!> @file gradsFacesJ.f90
!!
!! Computation of gradients at j-faces.
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
!! at the faces "j" of the grid cells.
!!
!! @param u  u-velocity (computed in GradsInitial)
!! @param v  v-velocity (computed in GradsInitial)
!!
subroutine GradsFacesJ( u,v )

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

  do i=2,il
    do j=2,j2

! --- bottom face of auxiliary control volume

      sx     = 0.5D0*(sj(1,i,j)+sj(1,i,j+1))
      sy     = 0.5D0*(sj(2,i,j)+sj(2,i,j+1))
      fgx(1) =    u(i,j)*sx
      fgx(2) =    v(i,j)*sx
      fgx(3) = dv(2,i,j)*sx
      fgy(1) =    u(i,j)*sy
      fgy(2) =    v(i,j)*sy
      fgy(3) = dv(2,i,j)*sy

      gradfj(1,i,j  ) = gradfj(1,i,j  ) - fgx(1)
      gradfj(2,i,j  ) = gradfj(2,i,j  ) - fgy(1)
      gradfj(3,i,j  ) = gradfj(3,i,j  ) - fgx(2)
      gradfj(4,i,j  ) = gradfj(4,i,j  ) - fgy(2)
      gradfj(5,i,j  ) = gradfj(5,i,j  ) - fgx(3)
      gradfj(6,i,j  ) = gradfj(6,i,j  ) - fgy(3)

      gradfj(1,i,j+1) = gradfj(1,i,j+1) + fgx(1)
      gradfj(2,i,j+1) = gradfj(2,i,j+1) + fgy(1)
      gradfj(3,i,j+1) = gradfj(3,i,j+1) + fgx(2)
      gradfj(4,i,j+1) = gradfj(4,i,j+1) + fgy(2)
      gradfj(5,i,j+1) = gradfj(5,i,j+1) + fgx(3)
      gradfj(6,i,j+1) = gradfj(6,i,j+1) + fgy(3)

! --- left face of auxiliary control volume

      sx     = 0.5D0*(si(1,i,j)+si(1,i,j-1))
      sy     = 0.5D0*(si(2,i,j)+si(2,i,j-1))
      uav    = 0.25D0*(u(i,j)+u(i,j-1)+u(i-1,j-1)+u(i-1,j))
      vav    = 0.25D0*(v(i,j)+v(i,j-1)+v(i-1,j-1)+v(i-1,j))
      tav    = 0.25D0*(dv(2,i,j)+dv(2,i,j-1)+dv(2,i-1,j-1)+dv(2,i-1,j))
      fgx(1) = uav*sx
      fgx(2) = vav*sx
      fgx(3) = tav*sx
      fgy(1) = uav*sy
      fgy(2) = vav*sy
      fgy(3) = tav*sy

      gradfj(1,i  ,j) = gradfj(1,i  ,j) + fgx(1)
      gradfj(2,i  ,j) = gradfj(2,i  ,j) + fgy(1)
      gradfj(3,i  ,j) = gradfj(3,i  ,j) + fgx(2)
      gradfj(4,i  ,j) = gradfj(4,i  ,j) + fgy(2)
      gradfj(5,i  ,j) = gradfj(5,i  ,j) + fgx(3)
      gradfj(6,i  ,j) = gradfj(6,i  ,j) + fgy(3)

      gradfj(1,i-1,j) = gradfj(1,i-1,j) - fgx(1)
      gradfj(2,i-1,j) = gradfj(2,i-1,j) - fgy(1)
      gradfj(3,i-1,j) = gradfj(3,i-1,j) - fgx(2)
      gradfj(4,i-1,j) = gradfj(4,i-1,j) - fgy(2)
      gradfj(5,i-1,j) = gradfj(5,i-1,j) - fgx(3)
      gradfj(6,i-1,j) = gradfj(6,i-1,j) - fgy(3)
    enddo

! - treat boundary j=2

    sx     = sj(1,i,2)
    sy     = sj(2,i,2)
    fgx(1) = 0.5D0*(   u(i,1)+   u(i,2))*sx
    fgx(2) = 0.5D0*(   v(i,1)+   v(i,2))*sx
    fgx(3) = 0.5D0*(dv(2,i,1)+dv(2,i,2))*sx
    fgy(1) = 0.5D0*(   u(i,1)+   u(i,2))*sy
    fgy(2) = 0.5D0*(   v(i,1)+   v(i,2))*sy
    fgy(3) = 0.5D0*(dv(2,i,1)+dv(2,i,2))*sy

    gradfj(1,i,2) = gradfj(1,i,2) + fgx(1)
    gradfj(2,i,2) = gradfj(2,i,2) + fgy(1)
    gradfj(3,i,2) = gradfj(3,i,2) + fgx(2)
    gradfj(4,i,2) = gradfj(4,i,2) + fgy(2)
    gradfj(5,i,2) = gradfj(5,i,2) + fgx(3)
    gradfj(6,i,2) = gradfj(6,i,2) + fgy(3)

! - treat boundary j=jl

    sx     = sj(1,i,jl)
    sy     = sj(2,i,jl)
    fgx(1) = 0.5D0*(   u(i,j2)+   u(i,jl))*sx
    fgx(2) = 0.5D0*(   v(i,j2)+   v(i,jl))*sx
    fgx(3) = 0.5D0*(dv(2,i,j2)+dv(2,i,jl))*sx
    fgy(1) = 0.5D0*(   u(i,j2)+   u(i,jl))*sy
    fgy(2) = 0.5D0*(   v(i,j2)+   v(i,jl))*sy
    fgy(3) = 0.5D0*(dv(2,i,j2)+dv(2,i,jl))*sy

    gradfj(1,i,jl) = gradfj(1,i,jl) - fgx(1)
    gradfj(2,i,jl) = gradfj(2,i,jl) - fgy(1)
    gradfj(3,i,jl) = gradfj(3,i,jl) - fgx(2)
    gradfj(4,i,jl) = gradfj(4,i,jl) - fgy(2)
    gradfj(5,i,jl) = gradfj(5,i,jl) - fgx(3)
    gradfj(6,i,jl) = gradfj(6,i,jl) - fgy(3)

    sx     = 0.5D0*si(1,i,j2)
    sy     = 0.5D0*si(2,i,j2)
    uav    = 0.25D0*(u(i,jl)+u(i,j2)+u(i-1,j2)+u(i-1,jl))
    vav    = 0.25D0*(v(i,jl)+v(i,j2)+v(i-1,j2)+v(i-1,jl))
    tav    = 0.25D0*(dv(2,i,jl)+dv(2,i,j2)+dv(2,i-1,j2)+dv(2,i-1,jl))
    fgx(1) = uav*sx
    fgx(2) = vav*sx
    fgx(3) = tav*sx
    fgy(1) = uav*sy
    fgy(2) = vav*sy
    fgy(3) = tav*sy

    gradfj(1,i  ,jl) = gradfj(1,i  ,jl) + fgx(1)
    gradfj(2,i  ,jl) = gradfj(2,i  ,jl) + fgy(1)
    gradfj(3,i  ,jl) = gradfj(3,i  ,jl) + fgx(2)
    gradfj(4,i  ,jl) = gradfj(4,i  ,jl) + fgy(2)
    gradfj(5,i  ,jl) = gradfj(5,i  ,jl) + fgx(3)
    gradfj(6,i  ,jl) = gradfj(6,i  ,jl) + fgy(3)

    gradfj(1,i-1,jl) = gradfj(1,i-1,jl) - fgx(1)
    gradfj(2,i-1,jl) = gradfj(2,i-1,jl) - fgy(1)
    gradfj(3,i-1,jl) = gradfj(3,i-1,jl) - fgx(2)
    gradfj(4,i-1,jl) = gradfj(4,i-1,jl) - fgy(2)
    gradfj(5,i-1,jl) = gradfj(5,i-1,jl) - fgx(3)
    gradfj(6,i-1,jl) = gradfj(6,i-1,jl) - fgy(3)
  enddo ! i

! divide by the volume of auxiliary cell

  do i=2,i2
    do j=3,j2
      rvol          = 2.D0/(vol(i,j)+vol(i,j-1))
      gradfj(1,i,j) = gradfj(1,i,j)*rvol
      gradfj(2,i,j) = gradfj(2,i,j)*rvol
      gradfj(3,i,j) = gradfj(3,i,j)*rvol
      gradfj(4,i,j) = gradfj(4,i,j)*rvol
      gradfj(5,i,j) = gradfj(5,i,j)*rvol
      gradfj(6,i,j) = gradfj(6,i,j)*rvol
    enddo

    rvol           = 2.D0/vol(i,2 )
    gradfj(1,i,2 ) = gradfj(1,i,2 )*rvol
    gradfj(2,i,2 ) = gradfj(2,i,2 )*rvol
    gradfj(3,i,2 ) = gradfj(3,i,2 )*rvol
    gradfj(4,i,2 ) = gradfj(4,i,2 )*rvol
    gradfj(5,i,2 ) = gradfj(5,i,2 )*rvol
    gradfj(6,i,2 ) = gradfj(6,i,2 )*rvol

    rvol           = 2.D0/vol(i,j2)
    gradfj(1,i,jl) = gradfj(1,i,jl)*rvol
    gradfj(2,i,jl) = gradfj(2,i,jl)*rvol
    gradfj(3,i,jl) = gradfj(3,i,jl)*rvol
    gradfj(4,i,jl) = gradfj(4,i,jl)*rvol
    gradfj(5,i,jl) = gradfj(5,i,jl)*rvol
    gradfj(6,i,jl) = gradfj(6,i,jl)*rvol
  enddo ! i

end subroutine GradsFacesJ

!> @file gradsInitial.f90
!!
!! Initialization of gradient computation.
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

!> Computes velocity components and initializes face gradients of u, v,
!! and T to zero. Velocity components are utilized later in GradFacesI/J
!! and also in FluxViscous.
!!
!! @param u  u-velocity
!! @param v  v-velocity
!!
subroutine GradsInitial( u,v )

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  implicit none

! parameters
  real(rtype) :: u(:,:), v(:,:)

! local variables
  integer :: i, j

! *****************************************************************************
! compute velocity components

  do j=1,jmax
    do i=1,imax
      u(i,j) = cv(2,i,j)/cv(1,i,j)
      v(i,j) = cv(3,i,j)/cv(1,i,j)
    enddo
  enddo

! set gradients to zero

  do j=0,jmax
    do i=0,imax
      gradfi(1:6,i,j) = 0.D0
      gradfj(1:6,i,j) = 0.D0
    enddo
  enddo

end subroutine GradsInitial

!> @file irsmoo.f90
!!
!! Central implicit residual smoothing.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: March 22, 2014
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

!> Applies central implicit smoothing method to the residuals.
!! Thomas algorithm is used to invert the tridiagonal matrices.
!!
!! @param work  work space for temporary array
!!
subroutine Irsmoo( work )

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  implicit none

! parameters
  real(rtype) :: work(:)

! local variables
  integer     :: i, j, im1, ip1, jm1, jp1
  real(rtype) :: t
  real(rtype), allocatable :: d(:,:)

! *****************************************************************************

  d = Reshape( work,(/imax, jmax/) )  ! temporary array

! solution of the tridiagonal system in i-direction

  do j=2,j2
    d(1,j)     = 0.D0
    rhs(1,1,j) = 0.D0
    rhs(2,1,j) = 0.D0
    rhs(3,1,j) = 0.D0
    rhs(4,1,j) = 0.D0
  enddo

  do i=2,i2
    im1 = i - 1
    do j=2,j2
      t          = 1.D0/(1.D0+2.D0*epsij(1,i,j)-epsij(1,i,j)*d(im1,j))
      d(i,j)     = t*epsij(1,i,j)
      rhs(1,i,j) = t*(rhs(1,i,j) + epsij(1,i,j)*rhs(1,im1,j))
      rhs(2,i,j) = t*(rhs(2,i,j) + epsij(1,i,j)*rhs(2,im1,j))
      rhs(3,i,j) = t*(rhs(3,i,j) + epsij(1,i,j)*rhs(3,im1,j))
      rhs(4,i,j) = t*(rhs(4,i,j) + epsij(1,i,j)*rhs(4,im1,j))
    enddo
  enddo

  do i=i2-1,2,-1
    ip1 = i + 1
    do j=2,j2
      rhs(1,i,j) = rhs(1,i,j) + d(i,j)*rhs(1,ip1,j)
      rhs(2,i,j) = rhs(2,i,j) + d(i,j)*rhs(2,ip1,j)
      rhs(3,i,j) = rhs(3,i,j) + d(i,j)*rhs(3,ip1,j)
      rhs(4,i,j) = rhs(4,i,j) + d(i,j)*rhs(4,ip1,j)
    enddo
  enddo

! solution of the tridiagonal system in j-direction

  do i=2,i2
    d(i,1)     = 0.D0
    rhs(1,i,1) = 0.D0
    rhs(2,i,1) = 0.D0
    rhs(3,i,1) = 0.D0
    rhs(4,i,1) = 0.D0
  enddo

  do j=2,j2
    jm1 = j - 1
    do i=2,i2
      t          = 1.D0/(1.D0+2.D0*epsij(2,i,j)-epsij(2,i,j)*d(i,jm1))
      d(i,j)     = t*epsij(2,i,j)
      rhs(1,i,j) = t*(rhs(1,i,j) + epsij(2,i,j)*rhs(1,i,jm1))
      rhs(2,i,j) = t*(rhs(2,i,j) + epsij(2,i,j)*rhs(2,i,jm1))
      rhs(3,i,j) = t*(rhs(3,i,j) + epsij(2,i,j)*rhs(3,i,jm1))
      rhs(4,i,j) = t*(rhs(4,i,j) + epsij(2,i,j)*rhs(4,i,jm1))
    enddo
  enddo

  do j=j2-1,2,-1
    jp1 = j + 1
    do i=2,i2
      rhs(1,i,j) = rhs(1,i,j) + d(i,j)*rhs(1,i,jp1)
      rhs(2,i,j) = rhs(2,i,j) + d(i,j)*rhs(2,i,jp1)
      rhs(3,i,j) = rhs(3,i,j) + d(i,j)*rhs(3,i,jp1)
      rhs(4,i,j) = rhs(4,i,j) + d(i,j)*rhs(4,i,jp1)
    enddo
  enddo

end subroutine Irsmoo

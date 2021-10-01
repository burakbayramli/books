!> @file allocateMemory.f90
!!
!! Memory allocation (conservative and dependent variables, grid geometry,
!! numerical variables).
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: February 27, 2014
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

!> Allocates memory for all arrays.
!!
subroutine AllocateMemory

  use ModGeometry
  use ModNumerics
  use ModPhysics
  use ModInterfaces, only : ErrorMessage
  implicit none

! local variables
  integer :: errFlag

! *****************************************************************************
! grid

  allocate( x(0:imax,0:jmax),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for x()" )

  allocate( y(0:imax,0:jmax),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for y()" )

! base flow variables

  allocate( cv(1:nconv,0:imax,0:jmax),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for cv()" )

  allocate( dv(1:ndepv,0:imax,0:jmax),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for dv()" )

! general numerical variables

  allocate( si(1:2,0:imax,0:jmax) ,stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for si()" )

  allocate( sj(1:2,0:imax,0:jmax) ,stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for sj()" )

  allocate( vol(0:imax,0:jmax),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for vol()" )

  allocate( cvold(1:nconv,0:imax,0:jmax),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for cvold()" )

  allocate( diss(1:nconv,0:imax,0:jmax),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for diss()" )

  allocate( rhs(1:nconv,0:imax,0:jmax),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for rhs()" )

  allocate( sri(0:imax,0:jmax),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for sri()" )

  allocate( srj(0:imax,0:jmax),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for srj()" )

  allocate( tstep(0:imax,0:jmax),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for tstep()" )

! numerical variables required for higher-order Roe scheme

  if ((kdissip=="r" .or. kdissip=="R")) then
    allocate( dui(1:4,0:imax,0:jmax),stat=errFlag )
    if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for dui()" )

    allocate( duj(1:4,0:imax,0:jmax),stat=errFlag )
    if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for duj()" )
  endif

! numerical variables required for viscous flow

  if (kequs=="n" .or. kequs=="N") then
    allocate( gradfi(1:6,0:imax,0:jmax),stat=errFlag )
    if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for gradfi()" )

    allocate( gradfj(1:6,0:imax,0:jmax),stat=errFlag )
    if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for gradfj()" )
  endif

! numerical variables required for implicit residual smoothing

  if (epsirs > 0.D0) then
    allocate( epsij(1:2,0:imax,0:jmax),stat=errFlag )
    if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for epsij()" )
  endif

end subroutine AllocateMemory

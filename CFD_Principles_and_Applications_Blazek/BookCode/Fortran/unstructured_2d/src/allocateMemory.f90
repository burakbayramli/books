!> @file allocateMemory.f90
!!
!! Memory allocation for remaining arrays.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: May 23, 2014
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

!> Allocates memory for conservative, dependent and for numerical variables.
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
! base flow variables

  allocate( cv(nconv,nnodes),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for cv()" )

  allocate( dv(ndepv,nnodes),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for dv()" )

! general numerical variables

  allocate( cvold(nconv,nnodes),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for cvold()" )

  allocate( diss(nconv,nnodes),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for diss()" )

  allocate( rhs(nconv,nnodes),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for rhs()" )

  allocate( tstep(nnodes),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for tstep()" )

! numerical variables required for higher-order Roe scheme
! and for viscous flow

  if (iorder > 1) then
    allocate( lim(nconv,nnodes),stat=errFlag )
    if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for lim()" )

    if (kequs=="n" .or. kequs=="N") then
      allocate( gradx(5,nnodes),stat=errFlag )
      if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for gradx()" )
      allocate( grady(5,nnodes),stat=errFlag )
      if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for grady()" )
    else
      allocate( gradx(4,nnodes),stat=errFlag )
      if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for gradx()" )
      allocate( grady(4,nnodes),stat=errFlag )
      if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for grady()" )
    endif
  else
    if (kequs=="n" .or. kequs=="N") then
      allocate( gradx(5,nnodes),stat=errFlag )
      if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for gradx()" )
      allocate( grady(5,nnodes),stat=errFlag )
      if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for grady()" )
    endif
  endif

end subroutine AllocateMemory

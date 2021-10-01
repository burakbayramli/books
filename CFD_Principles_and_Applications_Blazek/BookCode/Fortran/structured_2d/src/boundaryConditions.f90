!> @file boundaryConditions.f90
!!
!! Application of boundary conditions.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: March 6, 2014
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

!> Sets boundary conditions in dummy cells. In a first loop, b.c.'s at
!! inlet, outlet, far-field and cuts are specified. In a second loop,
!! b.c.'s are set for all solid walls and symmetry planes.
!!
!! @param work  work space for temporary variables (used by far-field b.c.)
!!
subroutine BoundaryConditions( work )

  use ModDataTypes
  use ModGeometry
  use ModPhysics
  use ModInterfaces, only : BcondCut, BcondFarfield, BcondInflow, BcondInject, &
                            BcondOutflow, BcondSymmetry, BcondWalleu, &
                            BcondWallns, DependentVarsOne, ErrorMessage
  implicit none

! parameters
  real(rtype) :: work(:)

! local variables
  integer :: itype, lb, lbeg, lend, lbs, lbegs, lends, mp, wdim
  integer :: iseg, neq

! *****************************************************************************
! fill dummy cells at the corners: (1,1), (il,1), (1,jl), (il,jl)

  do neq=1,nconv
    cv(neq, 1, 1) = 0.5D0*(cv(neq, 1, 2)+cv(neq, 2, 1))
    cv(neq,il, 1) = 0.5D0*(cv(neq,il, 2)+cv(neq,i2, 1))
    cv(neq, 1,jl) = 0.5D0*(cv(neq, 1,j2)+cv(neq, 2,jl))
    cv(neq,il,jl) = 0.5D0*(cv(neq,il,j2)+cv(neq,i2,jl))
  enddo
  call DependentVarsOne(  1, 1 )
  call DependentVarsOne( il, 1 )
  call DependentVarsOne(  1,jl )
  call DependentVarsOne( il,jl )

! loop over all boundaries with the exception of walls and symmetry planes

  do iseg=1,nsegs

    itype = lbsegs(iseg,1)
    lb    = lbsegs(iseg,2)
    lbeg  = lbsegs(iseg,3)
    lend  = lbsegs(iseg,4)

! - inflow

    if (itype>=100 .and. itype<200) then

      call BcondInflow( lb,lbeg,lend )

! - outflow

    else if (itype>=200 .and. itype<300) then

      call BcondOutflow( lb,lbeg,lend )

! - far-field

    else if (itype>=600 .and. itype<700) then

      mp   = Max(imax,jmax) + 1
      wdim = Ubound(work,1)
      if ((4*mp) > wdim) then
        call ErrorMessage( "insufficient work space in BoundaryConditions" )
      endif
      call BcondFarfield( lb,lbeg,lend, &
                          work( 1      :  mp), &
                          work((1+  mp):2*mp), &
                          work((1+2*mp):3*mp), &
                          work((1+3*mp):4*mp) )

! - cut / periodic

    else if (itype>=700 .and. itype<800) then

      lbs   = lbsegs(iseg,5)
      lbegs = lbsegs(iseg,6)
      lends = lbsegs(iseg,7)
      call BcondCut( lb,lbeg,lend,lbs,lbegs,lends,nconv,cv )
      call BcondCut( lb,lbeg,lend,lbs,lbegs,lends,ndepv,dv )

! - injection

    else if (itype>=800 .and. itype<900) then

      call BcondInject( lb,lbeg,lend )

    endif

  enddo ! iseg

! solid walls and symmetry (treated last because they should dominate)

  do iseg=1,nsegs

    itype = lbsegs(iseg,1)
    lb    = lbsegs(iseg,2)
    lbeg  = lbsegs(iseg,3)
    lend  = lbsegs(iseg,4)

! - viscous (no-slip) wall - if Navier-Stokes equations solved

    if (itype>=300 .and. itype<400) then

      if (kequs == "E") then
        call BcondWalleu( lb,lbeg,lend )
      else
        call BcondWallns( lb,lbeg,lend )
      endif

! - Euler (slip) wall

    else if (itype>=400 .and. itype<500) then

      call BcondWalleu( lb,lbeg,lend )

! - symmetry

    else if (itype>=500 .and. itype<600) then

      call BcondSymmetry( lb,lbeg,lend )

    endif

  enddo ! iseg

end subroutine BoundaryConditions

!> @file bcondSymmetry.f90
!!
!! Treatment of symmetry boundaries.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: March 10, 2014
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

!> Applies symmetry boundary condition to dummy cells:\n
!! (1) density and energy are set equal to values in the corresponding
!!     physical cells\n
!! (2) velocity components are mirrored with respect to the boundary.
!!     It is assumed the boundary coincides either with x=const. or
!!     y=const. line.\n
!! Values are prescribed in the first layer and extrapolated to the second
!! dummy layer.
!!
!! @param lb    side of the computational domain (1: j=2, 2: i=i2, 3: j=j2,4: i=2)
!! @param lbeg  start index of the boundary segment
!! @param lend  end index of the boundary segment
!!
subroutine BcondSymmetry( lb,lbeg,lend )

  use ModDataTypes
  use ModGeometry
  use ModPhysics
  use ModInterfaces, only : DependentVarsOne
  implicit none

! parameters
  integer, intent(in) :: lb, lbeg, lend

! local variables
  integer :: iins1, iins2, idum1, idum2, jins1, jins2, &
             jdum1, jdum2, lbegu, lendu, lstep
  integer :: i, j

! *****************************************************************************

  if      (lb == 1) then
    jins1 = 2
    jins2 = 3
    jdum1 = 1
    jdum2 = 0
  else if (lb == 2) then
    iins1 = i2
    iins2 = i2 - 1
    idum1 = il
    idum2 = imax
  else if (lb == 3) then
    jins1 = j2
    jins2 = j2 - 1
    jdum1 = jl
    jdum2 = jmax
  else if (lb == 4) then
    iins1 = 2
    iins2 = 3
    idum1 = 1
    idum2 = 0
  endif

                   lstep =  1
  if (lbeg > lend) lstep = -1

! boundary j=2 / j=j2 and i=lbeg,lend

  if (lb==1 .or. lb==3) then

                    lbegu = lbeg
                    lendu = lend
    if (lbeg ==  2) lbegu = 1     ! extend to dummy cells (1st layer)
    if (lend == i2) lendu = il

    do i=lbegu,lendu,lstep
      cv(1,i,jdum1) =  cv(1,i,jins1)
      cv(1,i,jdum2) =  cv(1,i,jins2)
      cv(2,i,jdum1) =  cv(2,i,jins1)
      cv(2,i,jdum2) =  cv(2,i,jins2)
      cv(3,i,jdum1) = -cv(3,i,jins1)
      cv(3,i,jdum2) = -cv(3,i,jins2)
      cv(4,i,jdum1) =  cv(4,i,jins1)
      cv(4,i,jdum2) =  cv(4,i,jins2)

      call DependentVarsOne( i,jdum1 )
      call DependentVarsOne( i,jdum2 )
    enddo

! boundary i=2 / i=i2 and j=lbeg,lend

  else if (lb==2 .or. lb==4) then

                    lbegu = lbeg
                    lendu = lend
    if (lbeg ==  2) lbegu = 1     ! extend to dummy cells (1st layer)
    if (lend == j2) lendu = jl

    do j=lbegu,lendu,lstep
      cv(1,idum1,j) =  cv(1,iins1,j)
      cv(1,idum2,j) =  cv(1,iins2,j)
      cv(2,idum1,j) = -cv(2,iins1,j)
      cv(2,idum2,j) = -cv(2,iins2,j)
      cv(3,idum1,j) =  cv(3,iins1,j)
      cv(3,idum2,j) =  cv(3,iins2,j)
      cv(4,idum1,j) =  cv(4,iins1,j)
      cv(4,idum2,j) =  cv(4,iins2,j)

      call DependentVarsOne( idum1,j )
      call DependentVarsOne( idum2,j )
    enddo

  endif

end subroutine BcondSymmetry

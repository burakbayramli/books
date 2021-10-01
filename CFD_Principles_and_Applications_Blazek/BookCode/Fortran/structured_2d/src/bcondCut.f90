!> @file bcondCut.f90
!!
!! Treatment of interior cuts and periodic boundaries.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: March 17, 2014
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

!> Sets boundary conditions at cuts / periodic boundaries by exchanging
!! the quantity (var) between the dummy and the real cells. It is assumed
!! that both segments (current and source) extend in the same coordinate
!! direction (e.g., i=const).
!!
!! @param lb     side of the computational domain (1: j=2, 2: i=i2, 3: j=j2,4: i=2)
!! @param lbeg   start index of the boundary segment
!! @param lend   end index of the boundary segment
!! @param lbs    side of the computational domain (source segment)
!! @param lbegs  start index of the source segment
!! @param lends  end index of the source segment
!! @param nvars  number of variables to exchange (1st dimension of var)
!! @param var    variables to exchange
!!
subroutine BcondCut( lb,lbeg,lend,lbs,lbegs,lends,nvars,var )

  use ModDataTypes
  use ModGeometry
  implicit none

! parameters
  integer, intent(in) :: lb, lbeg, lend, lbs, lbegs, lends, nvars
  real(rtype) :: var(:,0:,0:)

! local variables
  integer :: i, is, j, js
  integer :: iins1, iins2, idum1, idum2, jins1, jins2, jdum1, jdum2, &
             lstep, lsteps

! *****************************************************************************

  if      (lb == 1) then
    jdum1 = 1
    jdum2 = 0
  else if (lb == 2) then
    idum1 = il
    idum2 = imax
  else if (lb == 3) then
    jdum1 = jl
    jdum2 = jmax
  else if (lb == 4) then
    idum1 = 1
    idum2 = 0
  endif

  if      (lbs == 1) then
    jins1 = 2
    jins2 = 3
  else if (lbs == 2) then
    iins1 = i2
    iins2 = i2 - 1
  else if (lbs == 3) then
    jins1 = j2
    jins2 = j2 - 1
  else if (lbs == 4) then
    iins1 = 2
    iins2 = 3
  endif

                     lstep  =  1
                     lsteps =  1
  if (lbeg  > lend ) lstep  = -1
  if (lbegs > lends) lsteps = -1

! boundary j=2 / j=j2 and i=lbeg,lend

  if (lb==1 .or. lb==3) then
    is = lbegs
    do i=lbeg,lend,lstep
      var(1:nvars,i,jdum1) = var(1:nvars,is,jins1)
      var(1:nvars,i,jdum2) = var(1:nvars,is,jins2)
      is = is + lsteps
    enddo

! boundary i=2 / i=i2 and j=lbeg,lend

  else if (lb==2 .or. lb==4) then
    js = lbegs
    do j=lbeg,lend,lstep
      var(1:nvars,idum1,j) = var(1:nvars,iins1,js)
      var(1:nvars,idum2,j) = var(1:nvars,iins2,js)
      js = js + lsteps
    enddo
  endif

end subroutine BcondCut

! =============================================================================

!> Exchanges single variable at cuts / periodic boundaries between the dummy
!! and the real cells. It is assumed that both segments (current and source)
!! extend in the same coordinate direction (e.g., i=const). This function is
!! utilized for cell volumes and spectral radii.
!!
!! @param lb     side of the computational domain (1: j=2, 2: i=i2, 3: j=j2,4: i=2)
!! @param lbeg   start index of the boundary segment
!! @param lend   end index of the boundary segment
!! @param lbs    side of the computational domain (source segment)
!! @param lbegs  start index of the source segment
!! @param lends  end index of the source segment
!! @param var    variable to exchange
!!
subroutine BcondCutSingle( lb,lbeg,lend,lbs,lbegs,lends,var )

  use ModDataTypes
  use ModGeometry
  implicit none

! parameters
  integer, intent(in) :: lb, lbeg, lend, lbs, lbegs, lends
  real(rtype) :: var(0:,0:)

! local variables
  integer :: i, is, j, js
  integer :: iins1, iins2, idum1, idum2, jins1, jins2, jdum1, jdum2, &
             lstep, lsteps

! *****************************************************************************

  if      (lb == 1) then
    jdum1 = 1
    jdum2 = 0
  else if (lb == 2) then
    idum1 = il
    idum2 = imax
  else if (lb == 3) then
    jdum1 = jl
    jdum2 = jmax
  else if (lb == 4) then
    idum1 = 1
    idum2 = 0
  endif

  if      (lbs == 1) then
    jins1 = 2
    jins2 = 3
  else if (lbs == 2) then
    iins1 = i2
    iins2 = i2 - 1
  else if (lbs == 3) then
    jins1 = j2
    jins2 = j2 - 1
  else if (lbs == 4) then
    iins1 = 2
    iins2 = 3
  endif

                     lstep  =  1
                     lsteps =  1
  if (lbeg  > lend ) lstep  = -1
  if (lbegs > lends) lsteps = -1

! boundary j=2 / j=j2 and i=lbeg,lend

  if (lb==1 .or. lb==3) then
    is = lbegs
    do i=lbeg,lend,lstep
      var(i,jdum1) = var(is,jins1)
      var(i,jdum2) = var(is,jins2)
      is = is + lsteps
    enddo

! boundary i=2 / i=i2 and j=lbeg,lend

  else if (lb==2 .or. lb==4) then
    js = lbegs
    do j=lbeg,lend,lstep
      var(idum1,j) = var(iins1,js)
      var(idum2,j) = var(iins2,js)
      js = js + lsteps
    enddo
  endif

end subroutine BcondCutSingle

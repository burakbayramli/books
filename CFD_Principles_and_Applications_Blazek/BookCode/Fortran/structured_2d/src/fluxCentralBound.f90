!> @file fluxCentralBound.f90
!!
!! Computation of central boundary fluxes.
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

!> Computes convective fluxes at boundaries using average of variables
!! at cell faces. Subroutine treats all boundaries except solid walls,
!! injection and symmetry planes.
!!
!! @param lb     side of the computational domain (1: j=2, 2: i=i2, 3: j=j2,4: i=2)
!! @param lbeg   start index of the boundary segment
!! @param lend   end index of the boundary segment
!! @param itype  type of boundary condition
!!
subroutine FluxCentralBound( lb,lbeg,lend,itype )

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  implicit none

! parameters
  integer, intent(in) :: lb, lbeg, lend, itype

! local variables
  integer     :: lstep, iins1, jins1, idum1, jdum1
  integer     :: i, j
  real(rtype) :: sx, sy, rhoa, rhoua, rhova, rhoea, pa, vcont

! *****************************************************************************

  if      (lb == 1) then
    jins1 = 2
    jdum1 = 1
  else if (lb == 2) then
    iins1 = i2
    idum1 = il
  else if (lb == 3) then
    jins1 = j2
    jdum1 = jl
  else if (lb == 4) then
    iins1 = 2
    idum1 = 1
  endif
                   lstep =  1
  if (lbeg > lend) lstep = -1

! boundary j=2 / j=j2 and i=lbeg,lend -----------------------------------------
! (flux in j-direction)

  if (lb==1 .or. lb==3) then

    if ((itype>=100 .and. itype<300) .or. &
        (itype>=600 .and. itype<800) .or. &
        (itype>=900)) then

      do i=lbeg,lend,lstep
        if (lb == 1) then
          sx =  sj(1,i,jins1)
          sy =  sj(2,i,jins1)
        else
          sx = -sj(1,i,jdum1)
          sy = -sj(2,i,jdum1)
        endif
        rhoa  = 0.5D0*(cv(1,i,jdum1)+cv(1,i,jins1))
        rhoua = 0.5D0*(cv(2,i,jdum1)+cv(2,i,jins1))
        rhova = 0.5D0*(cv(3,i,jdum1)+cv(3,i,jins1))
        rhoea = 0.5D0*(cv(4,i,jdum1)+cv(4,i,jins1))
        pa    = 0.5D0*(dv(1,i,jdum1)+dv(1,i,jins1))
        vcont = (rhoua*sx+rhova*sy)/rhoa

        rhs(1,i,jins1) = rhs(1,i,jins1) + vcont*rhoa
        rhs(2,i,jins1) = rhs(2,i,jins1) + vcont*rhoua + pa*sx
        rhs(3,i,jins1) = rhs(3,i,jins1) + vcont*rhova + pa*sy
        rhs(4,i,jins1) = rhs(4,i,jins1) + vcont*(rhoea+pa)
      enddo

    endif

! boundary i=2 / i=i2 and j=lbeg,lend -----------------------------------------
! (flux in i-direction)

  else if (lb==2 .or. lb==4) then

    if ((itype>=100 .and. itype<300) .or. &
        (itype>=600 .and. itype<800) .or. &
        (itype>=900)) then

      do j=lbeg,lend,lstep
        if (lb == 4) then
          sx =  si(1,iins1,j)
          sy =  si(2,iins1,j)
        else
          sx = -si(1,idum1,j)
          sy = -si(2,idum1,j)
        endif
        rhoa  = 0.5D0*(cv(1,idum1,j)+cv(1,iins1,j))
        rhoua = 0.5D0*(cv(2,idum1,j)+cv(2,iins1,j))
        rhova = 0.5D0*(cv(3,idum1,j)+cv(3,iins1,j))
        rhoea = 0.5D0*(cv(4,idum1,j)+cv(4,iins1,j))
        pa    = 0.5D0*(dv(1,idum1,j)+dv(1,iins1,j))
        vcont = (rhoua*sx+rhova*sy)/rhoa

        rhs(1,iins1,j) = rhs(1,iins1,j) + vcont*rhoa
        rhs(2,iins1,j) = rhs(2,iins1,j) + vcont*rhoua + pa*sx
        rhs(3,iins1,j) = rhs(3,iins1,j) + vcont*rhova + pa*sy
        rhs(4,iins1,j) = rhs(4,iins1,j) + vcont*(rhoea+pa)
      enddo

    endif

  endif ! lb

end subroutine FluxCentralBound

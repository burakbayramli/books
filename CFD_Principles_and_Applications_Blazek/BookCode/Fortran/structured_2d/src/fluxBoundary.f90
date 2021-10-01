!> @file fluxBoundary.f90
!!
!! Computation of general boundary fluxes.
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

!> Computes convective fluxes at boundaries of the type: wall, symmetry
!! and injection. Note that there is no difference between central and
!! upwind averaging for these boundaries.
!!
!! @param lb     side of the computational domain (1: j=2, 2: i=i2, 3: j=j2,4: i=2)
!! @param lbeg   start index of the boundary segment
!! @param lend   end index of the boundary segment
!! @param itype  type of boundary condition
!!
subroutine FluxBoundary( lb,lbeg,lend,itype )

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  implicit none

! parameters
  integer, intent(in) :: lb, lbeg, lend, itype

! local variables
  integer :: lstep, iins1, iins2, iins3, jins1, jins2, jins3, idum1, jdum1
  integer :: i, j
  real(rtype) :: sx, sy, ds, rhoa, rhoua, rhova, rhoea, pa, vcont, gam1, rgas

! *****************************************************************************

  if      (lb == 1) then
    jins1 = 2
    jins2 = 3
    jins3 = 4
    jdum1 = 1
  else if (lb == 2) then
    iins1 = i2
    iins2 = i2 - 1
    iins3 = i2 - 2
    idum1 = il
  else if (lb == 3) then
    jins1 = j2
    jins2 = j2 - 1
    jins3 = j2 - 2
    jdum1 = jl
  else if (lb == 4) then
    iins1 = 2
    iins2 = 3
    iins3 = 4
    idum1 = 1
  endif
                   lstep =  1
  if (lbeg > lend) lstep = -1

! boundary j=2 / j=j2 and i=lbeg,lend -----------------------------------------
! (flux in j-direction)

  if (lb==1 .or. lb==3) then

! - noslip (viscous) wall

    if (itype>=300 .and. itype<400 .and. (kequs=="N")) then

      do i=lbeg,lend,lstep
        if (lb == 1) then
          sx =  sj(1,i,jins1)
          sy =  sj(2,i,jins1)
        else
          sx = -sj(1,i,jdum1)
          sy = -sj(2,i,jdum1)
        endif
        pa = dv(1,i,jins1)

        rhs(2,i,jins1) = rhs(2,i,jins1) + pa*sx
        rhs(3,i,jins1) = rhs(3,i,jins1) + pa*sy
      enddo

! - slip (Euler) wall

    else if ((itype>=400 .and. itype<500) .or. &
             (itype>=300 .and. itype<400 .and. (kequs=="E"))) then

      do i=lbeg,lend,lstep
        if (lb == 1) then
          sx =  sj(1,i,jins1)
          sy =  sj(2,i,jins1)
        else
          sx = -sj(1,i,jdum1)
          sy = -sj(2,i,jdum1)
        endif
        if (iextrapol < 3) then
          pa = 0.5D0*(3.D0*dv(1,i,jins1)-dv(1,i,jins2))
        else
          pa = (15.D0*dv(1,i,jins1)-10.D0*dv(1,i,jins2)+3.D0*dv(1,i,jins3))/8.D0
        endif

        rhs(2,i,jins1) = rhs(2,i,jins1) + pa*sx
        rhs(3,i,jins1) = rhs(3,i,jins1) + pa*sy
      enddo

! - symmetry

    else if (itype>=500 .and. itype<600) then

      do i=lbeg,lend,lstep
        if (lb == 1) then
          sx =  sj(1,i,jins1)
          sy =  sj(2,i,jins1)
        else
          sx = -sj(1,i,jdum1)
          sy = -sj(2,i,jdum1)
        endif
        pa = 0.5D0*(dv(1,i,jdum1)+dv(1,i,jins1))

        rhs(2,i,jins1) = rhs(2,i,jins1) + pa*sx
        rhs(3,i,jins1) = rhs(3,i,jins1) + pa*sy
      enddo

! - injection

    else if (itype>=800 .and. itype<900) then

      do i=lbeg,lend,lstep
        if (lb == 1) then
          ds = Sqrt(sj(1,i,jins1)**2+sj(2,i,jins1)**2)
          sx =  sj(1,i,jins1)
          sy =  sj(2,i,jins1)
        else
          ds = Sqrt(sj(1,i,jdum1)**2+sj(2,i,jdum1)**2)
          sx = -sj(1,i,jdum1)
          sy = -sj(2,i,jdum1)
        endif
        gam1  = dv(4,i,jins1) - 1.D0
        rgas  = gam1*dv(5,i,jins1)/dv(4,i,jins1)
        pa    = 0.5D0*(3.D0*dv(1,i,jins1)-dv(1,i,jins2))
        rhoa  = pa/(rgas*tinject)
        rhoua = -sx*minject/ds
        rhova = -sy*minject/ds
        rhoea = pa/gam1 + 0.5D0*(rhoua*rhoua+rhova*rhova)/rhoa
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

! - noslip (viscous) wall

    if (itype>=300 .and. itype<400 .and. (kequs=="N")) then

      do j=lbeg,lend,lstep
        if (lb == 4) then
          sx =  si(1,iins1,j)
          sy =  si(2,iins1,j)
        else
          sx = -si(1,idum1,j)
          sy = -si(2,idum1,j)
        endif
        pa = dv(1,iins1,j)

        rhs(2,iins1,j) = rhs(2,iins1,j) + pa*sx
        rhs(3,iins1,j) = rhs(3,iins1,j) + pa*sy
      enddo

! - slip (Euler) wall

    else if ((itype>=400 .and. itype<500) .or. &
             (itype>=300 .and. itype<400 .and. (kequs=="E"))) then

      do j=lbeg,lend,lstep
        if (lb == 4) then
          sx =  si(1,iins1,j)
          sy =  si(2,iins1,j)
        else
          sx = -si(1,idum1,j)
          sy = -si(2,idum1,j)
        endif
        if (iextrapol < 3) then
          pa = 0.5D0*(3.D0*dv(1,iins1,j)-dv(1,iins2,j))
        else
          pa = (15.D0*dv(1,iins1,j)-10.D0*dv(1,iins2,j)+3.D0*dv(1,iins3,j))/8.D0
        endif

        rhs(2,iins1,j) = rhs(2,iins1,j) + pa*sx
        rhs(3,iins1,j) = rhs(3,iins1,j) + pa*sy
      enddo

! - symmetry

    else if (itype>=500 .and. itype<600) then

      do j=lbeg,lend,lstep
        if (lb == 4) then
          sx =  si(1,iins1,j)
          sy =  si(2,iins1,j)
        else
          sx = -si(1,idum1,j)
          sy = -si(2,idum1,j)
        endif
        pa = 0.5D0*(dv(1,idum1,j)+dv(1,iins1,j))

        rhs(2,iins1,j) = rhs(2,iins1,j) + pa*sx
        rhs(3,iins1,j) = rhs(3,iins1,j) + pa*sy
      enddo

! - injection

    else if (itype>=800 .and. itype<900) then

      do j=lbeg,lend,lstep
        if (lb == 4) then
          ds = Sqrt(si(1,iins1,j)**2+si(2,iins1,j)**2)
          sx =  si(1,iins1,j)
          sy =  si(2,iins1,j)
        else
          ds = Sqrt(si(1,idum1,j)**2+si(2,idum1,j)**2)
          sx = -si(1,idum1,j)
          sy = -si(2,idum1,j)
        endif
        gam1  = dv(4,iins1,j) - 1.D0
        rgas  = gam1*dv(5,iins1,j)/dv(4,iins1,j)
        pa    = 0.5D0*(3.D0*dv(1,iins1,j)-dv(1,iins2,j))
        rhoa  = pa/(rgas*tinject)
        rhoua = -sx*minject/ds
        rhova = -sy*minject/ds
        rhoea = pa/gam1 + 0.5D0*(rhoua*rhoua+rhova*rhova)/rhoa
        vcont = (rhoua*sx+rhova*sy)/rhoa

        rhs(1,iins1,j) = rhs(1,iins1,j) + vcont*rhoa
        rhs(2,iins1,j) = rhs(2,iins1,j) + vcont*rhoua + pa*sx
        rhs(3,iins1,j) = rhs(3,iins1,j) + vcont*rhova + pa*sy
        rhs(4,iins1,j) = rhs(4,iins1,j) + vcont*(rhoea+pa)
      enddo

    endif ! itype

  endif ! lb

end subroutine FluxBoundary

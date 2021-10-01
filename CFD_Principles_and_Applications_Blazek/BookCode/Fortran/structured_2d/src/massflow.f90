!> @file massflow.f90
!!
!! Computation of mass flow.
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

!> Computes mass flow at inlet and mass flow ratio between inlet and outlet
!! boundaries. Contributions are summed up by looping over ALL in-/outlet and
!! injection boundaries.
!!
subroutine Massflow

  use ModDataTypes
  use ModGeometry
  use ModPhysics
  use ModPlotQuant
  implicit none

! local variables
  integer     :: itype, lb, lbeg, lend, lstep, iins1, jins1, &
                 idum1, jdum1, in, out
  integer     :: i, j, iseg
  real(rtype) :: sx, sy, massin, massout, mass

! *****************************************************************************
! initialize mass flow before summing up the contributions

  massin  = 0.D0
  massout = 0.D0
  mflow   = 0.D0
  mfratio = 0.D0

  in  = 0  ! flow into the domain (=1)
  out = 0  ! flow out of the domain (=1)

! loop over boundaries searching for inlet / outlet / injection

  do iseg=1,nsegs

    itype = lbsegs(iseg,1)
    lb    = lbsegs(iseg,2)
    lbeg  = lbsegs(iseg,3)
    lend  = lbsegs(iseg,4)

    if ((itype>=100 .and. itype<300) .or. &
        (itype>=800 .and. itype<900)) then

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

! --- boundary j=2 / j=j2 and i=lbeg,lend

      if (lb==1 .or. lb==3) then

        do i=lbeg,lend,lstep
          if (lb == 1) then
            sx =  sj(1,i,jins1)
            sy =  sj(2,i,jins1)
          else
            sx = -sj(1,i,jdum1)
            sy = -sj(2,i,jdum1)
          endif
          mass = 0.5D0*(cv(2,i,jins1)+cv(2,i,jdum1))*sx + &
                 0.5D0*(cv(3,i,jins1)+cv(3,i,jdum1))*sy
          if (itype>=100 .and. itype<200) then
! --------- inflow
            massin  = massin - mass
            in = 1
          else if (itype>=200 .and. itype<300) then
! --------- outflow
            massout = massout + mass
            out = 1
          else
! --------- injection: in or out?
            if (minject >= 0.D0) then
              massin  = massin - mass
              in = 1
            else
              massout = massout + mass
              out = 1
            endif
          endif
        enddo

! --- boundary i=2 / i=i2 and j=lbeg,lend

      else if (lb==2 .or. lb==4) then

        do j=lbeg,lend,lstep
          if (lb == 4) then
            sx =  si(1,iins1,j)
            sy =  si(2,iins1,j)
          else
            sx = -si(1,idum1,j)
            sy = -si(2,idum1,j)
          endif
          mass = 0.5D0*(cv(2,iins1,j)+cv(2,idum1,j))*sx + &
                 0.5D0*(cv(3,iins1,j)+cv(3,idum1,j))*sy
          if (itype>=100 .and. itype<200) then
! --------- inflow
            massin  = massin - mass
            in = 1
          else if (itype>=200 .and. itype<300) then
! --------- outflow
            massout = massout + mass
            out = 1
          else
! --------- injection: in or out?
            if (minject >= 0.D0) then
              massin  = massin - mass
              in = 1
            else
              massout = massout + mass
              out = 1
            endif
          endif
        enddo

      endif

    endif ! itype

  enddo ! iseg

! mass flow and ratio

  if (in == 1) then
    mflow   = massin
    mfratio = massout/massin
  endif
  if (in==0 .and. out==1) then
    mflow   = massout
    mfratio = 1.D0
  endif

end subroutine Massflow

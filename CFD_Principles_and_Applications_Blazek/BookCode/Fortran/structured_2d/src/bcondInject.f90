!> @file bcondInject.f90
!!
!! Treatment of mass-injection boundaries.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: March 8, 2014
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

!> Applies injection boundary condition to dummy cells. Values are prescribed
!! in the first layer and extrapolated to the second dummy layer.
!!
!! @param lb    side of the computational domain (1: j=2, 2: i=i2, 3: j=j2,4: i=2)
!! @param lbeg  start index of the boundary segment
!! @param lend  end index of the boundary segment
!!
subroutine BcondInject( lb,lbeg,lend )

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  use ModInterfaces, only : DependentVarsOne
  implicit none

! parameters
  integer, intent(in) :: lb, lbeg, lend

! local variables
  integer     :: iins1, idum1, idum2, jins1, jdum1, jdum2, lstep
  integer     :: i, j
  real(rtype) :: gam1, rgas, pinj, rinj, uinj, vinj, reinj, rho, rhoe, &
                 sxn, syn, ds

! *****************************************************************************

  if      (lb == 1) then
    jins1 = 2
    jdum1 = 1
    jdum2 = 0
  else if (lb == 2) then
    iins1 = i2
    idum1 = il
    idum2 = imax
  else if (lb == 3) then
    jins1 = j2
    jdum1 = jl
    jdum2 = jmax
  else if (lb == 4) then
    iins1 = 2
    idum1 = 1
    idum2 = 0
  endif

                   lstep =  1
  if (lbeg > lend) lstep = -1

! boundary j=2 / j=j2 and i=lbeg,lend

  if (lb==1 .or. lb==3) then

    do i=lbeg,lend,lstep
      if (lb == 1) then
        ds  = Sqrt(sj(1,i,jins1)**2+sj(2,i,jins1)**2)
        sxn = -sj(1,i,jins1)/ds
        syn = -sj(2,i,jins1)/ds
      else
        ds  = Sqrt(sj(1,i,jdum1)**2+sj(2,i,jdum1)**2)
        sxn = sj(1,i,jdum1)/ds
        syn = sj(2,i,jdum1)/ds
      endif
      gam1  = dv(4,i,jins1) - 1.D0
      rgas  = gam1*dv(5,i,jins1)/dv(4,i,jins1)
      pinj  = dv(1,i,jins1)
      rinj  = pinj/(rgas*tinject)
      uinj  = sxn*minject/rinj
      vinj  = syn*minject/rinj
      reinj = pinj/gam1 + 0.5D0*rinj*(uinj*uinj+vinj*vinj)

      rho   = 2.D0*rinj  - cv(1,i,jins1)
      rhoe  = 2.D0*reinj - cv(4,i,jins1)
      if (rho<=0.D0 .or. rhoe<=0.D0 .or. &
          Abs(cv(1,i,jdum1)-rho )>maxichg*cv(1,i,jdum1) .or. &
          Abs(cv(4,i,jdum1)-rhoe)>maxichg*cv(4,i,jdum1)) then
        cv(1,i,jdum1) = rinj
        cv(2,i,jdum1) = rinj*uinj
        cv(3,i,jdum1) = rinj*vinj
        cv(4,i,jdum1) = reinj
      else
        cv(1,i,jdum1) = 2.D0*rinj      - cv(1,i,jins1)
        cv(2,i,jdum1) = 2.D0*rinj*uinj - cv(2,i,jins1)
        cv(3,i,jdum1) = 2.D0*rinj*vinj - cv(3,i,jins1)
        cv(4,i,jdum1) = 2.D0*reinj     - cv(4,i,jins1)
      endif

      cv(1,i,jdum2) = 2.D0*cv(1,i,jdum1) - cv(1,i,jins1)
      cv(2,i,jdum2) = 2.D0*cv(2,i,jdum1) - cv(2,i,jins1)
      cv(3,i,jdum2) = 2.D0*cv(3,i,jdum1) - cv(3,i,jins1)
      cv(4,i,jdum2) = 2.D0*cv(4,i,jdum1) - cv(4,i,jins1)

      call DependentVarsOne( i,jdum1 )
      call DependentVarsOne( i,jdum2 )
    enddo

! boundary i=2 / i=i2 and j=lbeg,lend

  else if (lb==2 .or. lb==4) then

    do j=lbeg,lend,lstep
      if (lb == 4) then
        ds  = Sqrt(si(1,iins1,j)**2+si(2,iins1,j)**2)
        sxn = -si(1,iins1,j)/ds
        syn = -si(2,iins1,j)/ds
      else
        ds  = Sqrt(si(1,idum1,j)**2+si(2,idum1,j)**2)
        sxn = si(1,idum1,j)/ds
        syn = si(2,idum1,j)/ds
      endif
      gam1  = dv(4,iins1,j) - 1.D0
      rgas  = gam1*dv(5,iins1,j)/dv(4,iins1,j)
      pinj  = dv(1,iins1,j)
      rinj  = pinj/(rgas*tinject)
      vinj  = sxn*minject/rinj
      uinj  = syn*minject/rinj
      reinj = pinj/gam1 + 0.5D0*rinj*(uinj*uinj+vinj*vinj)

      rho   = 2.D0*rinj  - cv(1,iins1,j)
      rhoe  = 2.D0*reinj - cv(4,iins1,j)
      if (rho<=0.D0 .or. rhoe<=0.D0 .or. &
          Abs(cv(1,idum1,j)-rho )>maxichg*cv(1,idum1,j) .or. &
          Abs(cv(4,idum1,j)-rhoe)>maxichg*cv(4,idum1,j)) then
        cv(1,idum1,j) = rinj
        cv(2,idum1,j) = rinj*uinj
        cv(3,idum1,j) = rinj*vinj
        cv(4,idum1,j) = reinj
      else
        cv(1,idum1,j) = 2.D0*rinj      - cv(1,iins1,j)
        cv(2,idum1,j) = 2.D0*rinj*uinj - cv(2,iins1,j)
        cv(3,idum1,j) = 2.D0*rinj*vinj - cv(3,iins1,j)
        cv(4,idum1,j) = 2.D0*reinj     - cv(4,iins1,j)
      endif

      cv(1,idum2,j) = 2.D0*cv(1,idum1,j) - cv(1,iins1,j)
      cv(2,idum2,j) = 2.D0*cv(2,idum1,j) - cv(2,iins1,j)
      cv(3,idum2,j) = 2.D0*cv(3,idum1,j) - cv(3,iins1,j)
      cv(4,idum2,j) = 2.D0*cv(4,idum1,j) - cv(4,iins1,j)

      call DependentVarsOne( idum1,j )
      call DependentVarsOne( idum2,j )
    enddo

  endif

end subroutine BcondInject

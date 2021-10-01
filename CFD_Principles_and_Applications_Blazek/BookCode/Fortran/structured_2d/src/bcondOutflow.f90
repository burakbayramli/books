!> @file bcondOutflow.f90
!!
!! Treatment of outflow boundaries.
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

!> Applies outflow boundary condition to dummy cells. Sub- or supersonic
!! outflow is possible. Values are prescribed in the first layer and
!! extrapolated to the second dummy layer.
!!
!! @param lb    side of the computational domain (1: j=2, 2: i=i2, 3: j=j2,4: i=2)
!! @param lbeg  start index of the boundary segment
!! @param lend  end index of the boundary segment
!!
subroutine BcondOutflow( lb,lbeg,lend )

  use ModDataTypes
  use ModGeometry
  use ModPhysics
  use ModInterfaces, only : DependentVarsOne
  implicit none

! parameters
  integer, intent(in) :: lb, lbeg, lend

! local variables
  integer     :: iins1, idum1, idum2, jins1, jdum1, jdum2, lstep
  integer     :: i, j
  real(rtype) :: gam1, ds, sxn, syn, rrho, u, v, rrhoc, deltp, &
                 rhob, ub, vb, vnd, q, mach

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

! boundary j=2 / j=j2 and i=lbeg,lend -----------------------------------------

  if (lb==1 .or. lb==3) then

    do i=lbeg,lend,lstep
      if (lb == 1) then
        ds  = Sqrt(sj(1,i,jins1)**2+sj(2,i,jins1)**2)
        sxn = sj(1,i,jins1)/ds
        syn = sj(2,i,jins1)/ds
      else
        ds  = Sqrt(sj(1,i,jdum1)**2+sj(2,i,jdum1)**2)
        sxn = -sj(1,i,jdum1)/ds
        syn = -sj(2,i,jdum1)/ds
      endif
      gam1  = dv(4,i,jins1) - 1.D0
      rrho  = 1.D0/cv(1,i,jins1)
      u     = cv(2,i,jins1)*rrho
      v     = cv(3,i,jins1)*rrho
      q     = Sqrt(u*u+v*v)
      mach  = q/dv(3,i,jins1)

      if (mach < 1.D0) then
        rrhoc = rrho/dv(3,i,jins1)
        deltp = dv(1,i,jins1) - pout
        rhob  = cv(1,i,jins1) - deltp/(dv(3,i,jins1)*dv(3,i,jins1))
        ub    = u + sxn*deltp*rrhoc
        vb    = v + syn*deltp*rrhoc

! ----- special treatment to prevent "deltp" from changing the sign
!       of velocity components. This may happen for very small u, v.

        vnd = ub*sxn + vb*syn
        if (vnd < 0.D0) then
          ub = Sign(1.D0,u)*Max(Abs(ub),Abs(u))
          vb = Sign(1.D0,v)*Max(Abs(vb),Abs(v))
        endif
        cv(1,i,jdum1) = rhob
        cv(2,i,jdum1) = rhob*ub
        cv(3,i,jdum1) = rhob*vb
        cv(4,i,jdum1) = pout/gam1 + 0.5D0*rhob*(ub*ub+vb*vb)
      else
        cv(1,i,jdum1) = cv(1,i,jins1)
        cv(2,i,jdum1) = cv(2,i,jins1)
        cv(3,i,jdum1) = cv(3,i,jins1)
        cv(4,i,jdum1) = cv(4,i,jins1)
      endif

      cv(1,i,jdum2) = 2.D0*cv(1,i,jdum1) - cv(1,i,jins1)
      cv(2,i,jdum2) = 2.D0*cv(2,i,jdum1) - cv(2,i,jins1)
      cv(3,i,jdum2) = 2.D0*cv(3,i,jdum1) - cv(3,i,jins1)
      cv(4,i,jdum2) = 2.D0*cv(4,i,jdum1) - cv(4,i,jins1)

      call DependentVarsOne( i,jdum1 )
      call DependentVarsOne( i,jdum2 )
    enddo

! boundary i=2 / i=i2 and j=lbeg,lend -----------------------------------------

  else if (lb==2 .or. lb==4) then

    do j=lbeg,lend,lstep
      if (lb == 4) then
        ds  = Sqrt(si(1,iins1,j)**2+si(2,iins1,j)**2)
        sxn = si(1,iins1,j)/ds
        syn = si(2,iins1,j)/ds
      else
        ds  = Sqrt(si(1,idum1,j)**2+si(2,idum1,j)**2)
        sxn = -si(1,idum1,j)/ds
        syn = -si(2,idum1,j)/ds
      endif
      gam1  = dv(4,iins1,j) - 1.D0
      rrho  = 1.D0/cv(1,iins1,j)
      u     = cv(2,iins1,j)*rrho
      v     = cv(3,iins1,j)*rrho
      q     = Sqrt(u*u+v*v)
      mach  = q/dv(3,iins1,j)

      if (mach < 1.D0) then
        rrhoc = rrho/dv(3,iins1,j)
        deltp = dv(1,iins1,j) - pout
        rhob  = cv(1,iins1,j) - deltp/(dv(3,iins1,j)*dv(3,iins1,j))
        ub    = u + sxn*deltp*rrhoc
        vb    = v + syn*deltp*rrhoc

! ----- special treatment to prevent "deltp" from changing the sign
!       of velocity components. This may happen for very small u, v.

        vnd = ub*sxn + vb*syn
        if (vnd < 0.D0) then
          ub = Sign(1.D0,u)*Max(Abs(ub),Abs(u))
          vb = Sign(1.D0,v)*Max(Abs(vb),Abs(v))
        endif
        cv(1,idum1,j) = rhob
        cv(2,idum1,j) = rhob*ub
        cv(3,idum1,j) = rhob*vb
        cv(4,idum1,j) = pout/gam1 + 0.5D0*rhob*(ub*ub+vb*vb)
      else
        cv(1,idum1,j) = cv(1,iins1,j)
        cv(2,idum1,j) = cv(2,iins1,j)
        cv(3,idum1,j) = cv(3,iins1,j)
        cv(4,idum1,j) = cv(4,iins1,j)
      endif

      cv(1,idum2,j) = 2.D0*cv(1,idum1,j) - cv(1,iins1,j)
      cv(2,idum2,j) = 2.D0*cv(2,idum1,j) - cv(2,iins1,j)
      cv(3,idum2,j) = 2.D0*cv(3,idum1,j) - cv(3,iins1,j)
      cv(4,idum2,j) = 2.D0*cv(4,idum1,j) - cv(4,iins1,j)

      call DependentVarsOne( idum1,j )
      call DependentVarsOne( idum2,j )
    enddo

  endif

end subroutine BcondOutflow

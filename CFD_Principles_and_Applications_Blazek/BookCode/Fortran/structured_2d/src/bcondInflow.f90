!> @file bcondInflow.f90
!!
!! Treatment of inflow boundaries.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: March 7, 2014
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

!> Applies inflow boundary condition to dummy cells (subsonic flow assumed).
!! Values are prescribed in the first layer and extrapolated to the second
!! dummy layer.
!!
!! @param lb    side of the computational domain (1: j=2, 2: i=i2, 3: j=j2,4: i=2)
!! @param lbeg  start index of the boundary segment
!! @param lend  end index of the boundary segment
!!
subroutine BcondInflow( lb,lbeg,lend )

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
  real(rtype) :: ds, sxn, syn, rrho, u, v, uabs, unorm, cosa, c02, rinv, &
                 dis, cb, cc02, tb, pb, rhob, uabsb, ub, vb, gam1, ggm1, &
                 rgas

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
      ggm1  = dv(4,i,jins1)/gam1
      rgas  = gam1*dv(5,i,jins1)/dv(4,i,jins1)
      rrho  = 1.D0/cv(1,i,jins1)
      u     = cv(2,i,jins1)*rrho
      v     = cv(3,i,jins1)*rrho
      uabs  = Sqrt(u*u+v*v)
      unorm = u*sxn + v*syn
      if (uabs < 1.D-20) then
        cosa = 1.D0
      else
        cosa = -unorm/uabs
      endif

      c02  = dv(3,i,jins1)*dv(3,i,jins1) + 0.5D0*gam1*uabs*uabs
      rinv = unorm - 2.D0*dv(3,i,jins1)/gam1
      dis  = (gam1*cosa*cosa+2.D0)*c02/(gam1*rinv*rinv) - 0.5D0*gam1
      if (dis < 0.D0) then
        write(*,1000) lb,i,jins1,dis
        dis = 1.D-20
      endif
      cb    = -rinv*(gam1/(gam1*cosa*cosa+2.D0))*(1.D0+cosa*Sqrt(dis))
      cc02  = Min(cb*cb/c02, 1.D0)
      tb    = ttinl*cc02
      pb    = ptinl*(tb/ttinl)**ggm1
      rhob  = pb/(rgas*tb)
      uabsb = 2.D0*dv(5,i,jins1)*(ttinl-tb)
      uabsb = Sqrt(uabsb)
      ub    = uabsb*Cos(betainl)
      vb    = uabsb*Sin(betainl)

      cv(1,i,jdum1) = rhob
      cv(2,i,jdum1) = rhob*ub
      cv(3,i,jdum1) = rhob*vb
      cv(4,i,jdum1) = pb/gam1 + 0.5D0*rhob*(ub*ub+vb*vb)
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
      ggm1  = dv(4,iins1,j)/gam1
      rgas  = gam1*dv(5,iins1,j)/dv(4,iins1,j)
      rrho  = 1.D0/cv(1,iins1,j)
      u     = cv(2,iins1,j)*rrho
      v     = cv(3,iins1,j)*rrho
      uabs  = Sqrt(u*u+v*v)
      unorm = u*sxn + v*syn
      if (uabs < 1.D-20) then
        cosa = 1.D0
      else
        cosa = -unorm/uabs
      endif

      c02  = dv(3,iins1,j)*dv(3,iins1,j) + 0.5D0*gam1*uabs*uabs
      rinv = unorm - 2.D0*dv(3,iins1,j)/gam1
      dis  = (gam1*cosa*cosa+2.D0)*c02/(gam1*rinv*rinv) - 0.5D0*gam1
      if (dis < 0.D0) then
        write(*,1000) lb,iins1,j,dis
        dis = 1.D-20
      endif
      cb    = -rinv*(gam1/(gam1*cosa*cosa+2.D0))*(1.D0+cosa*Sqrt(dis))
      cc02  = Min(cb*cb/c02, 1.D0)
      tb    = ttinl*cc02
      pb    = ptinl*(tb/ttinl)**ggm1
      rhob  = pb/(rgas*tb)
      uabsb = 2.D0*dv(5,iins1,j)*(ttinl-tb)
      uabsb = Sqrt(uabsb)
      ub    = uabsb*Cos(betainl)
      vb    = uabsb*Sin(betainl)

      cv(1,idum1,j) = rhob
      cv(2,idum1,j) = rhob*ub
      cv(3,idum1,j) = rhob*vb
      cv(4,idum1,j) = pb/gam1 + 0.5D0*rhob*(ub*ub+vb*vb)
      cv(1,idum2,j) = 2.D0*cv(1,idum1,j) - cv(1,iins1,j)
      cv(2,idum2,j) = 2.D0*cv(2,idum1,j) - cv(2,iins1,j)
      cv(3,idum2,j) = 2.D0*cv(3,idum1,j) - cv(3,iins1,j)
      cv(4,idum2,j) = 2.D0*cv(4,idum1,j) - cv(4,iins1,j)

      call DependentVarsOne( idum1,j )
      call DependentVarsOne( idum2,j )
    enddo

  endif

1000  format(" Warning (BcondInflow): discriminant<0 at boundary ",I1, &
             ", i= ",I3,", j= ",I3,", d= ",1PE13.5)

end subroutine BcondInflow

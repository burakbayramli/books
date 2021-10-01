!> @file forces.f90
!!
!! Computation of forces and moments.
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

!> Computes pressure forces and moments acting on the body. The contributions
!! are summed up by looping over ALL walls and injection boundaries. Subroutine
!! also computes lift and drag coefficients.
!!
subroutine Forces

  use ModDataTypes
  use ModGeometry
  use ModPhysics
  use ModPlotQuant
  implicit none

! local variables
  integer     :: itype, lb, lbeg, lend, lstep, iins1, jins1, idum1, jdum1
  integer     :: i, j, iseg
  real(rtype) :: sx, sy, pwall, cp, xa, ya, dcx, dcy, cx, cy

! *****************************************************************************
! initialize force coefficients

  cx = 0.D0
  cy = 0.D0
  cm = 0.D0

! loop over boundaries searching for walls & injection

  do iseg=1,nsegs

    itype = lbsegs(iseg,1)
    lb    = lbsegs(iseg,2)
    lbeg  = lbsegs(iseg,3)
    lend  = lbsegs(iseg,4)

    if ((itype>=300 .and. itype<500) .or. &
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
          pwall = 0.5D0*(dv(1,i,jdum1)+dv(1,i,jins1))
          cp    = 2.D0*(pwall-pinf)/(rhoinf*qinf*qinf)
          xa    = (0.5D0*(x(i,jins1)+x(i+1,jins1))-xref)/cref
          ya    = (0.5D0*(y(i,jins1)+y(i+1,jins1))-yref)/cref
          dcy   = sy*cp
          dcx   = sx*cp
          cy    = cy + dcy
          cx    = cx + dcx
          cm    = cm + dcx*ya - dcy*xa
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
          pwall = 0.5D0*(dv(1,idum1,j)+dv(1,iins1,j))
          cp    = 2.D0*(pwall-pinf)/(rhoinf*qinf*qinf)
          xa    = (0.5D0*(x(iins1,j)+x(iins1,j+1))-xref)/cref
          ya    = (0.5D0*(y(iins1,j)+y(iins1,j+1))-yref)/cref
          dcy   = sy*cp
          dcx   = sx*cp
          cy    = cy + dcy
          cx    = cx + dcx
          cm    = cm + dcx*ya - dcy*xa
        enddo

      endif

    endif ! itype

  enddo ! iseg

! final lift and drag coefficients (pressure forces only!)

  cl = cy*Cos(alpha) - cx*Sin(alpha)
  cd = cy*Sin(alpha) + cx*Cos(alpha)

end subroutine Forces

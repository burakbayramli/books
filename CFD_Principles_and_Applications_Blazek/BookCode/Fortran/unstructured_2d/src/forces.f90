!> @file forces.f90
!!
!! Computation of forces and moments.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: May 28, 2014
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
!! are summed up by looping over ALL walls. Subroutine also computes lift and
!! drag coefficients.
!!
subroutine Forces

  use ModDataTypes
  use ModGeometry
  use ModPhysics
  use ModPlotQuant
  implicit none

! local variables
  integer     :: ib, ibf, ibegf, iendf, n1, n2
  real(rtype) :: sx, sy, pwall, cp, xa, ya, dcx, dcy, cx, cy

! *****************************************************************************
! initialize force coefficients

  cx = 0.D0
  cy = 0.D0
  cm = 0.D0

! loop over boundaries searching for walls

  ibegf = 1

  do ib=1,nsegs
    iendf = ibound(1,ib)

    if (btype(ib)>=300 .and. btype(ib)<500) then
      do ibf=ibegf,iendf
        n1    = bface(1,ibf)
        n2    = bface(2,ibf)
        sx    = sbf(1,ibf)
        sy    = sbf(2,ibf)
        pwall = 0.5D0*(dv(1,n1)+dv(1,n2))
        cp    = 2.D0*(pwall-pinf)/(rhoinf*qinf*qinf)
        xa    = (0.5D0*(x(n1)+x(n2))-xref)/cref
        ya    = (0.5D0*(y(n1)+y(n2))-yref)/cref
        dcy   = sy*cp
        dcx   = sx*cp
        cy    = cy + dcy
        cx    = cx + dcx
        cm    = cm + dcx*ya - dcy*xa
      enddo
    endif ! btype

    ibegf = iendf + 1
  enddo   ! ib

! final lift and drag coefficients (pressure forces only!)

  cl = cy*Cos(alpha) - cx*Sin(alpha)
  cd = cy*Sin(alpha) + cx*Cos(alpha)

end subroutine Forces

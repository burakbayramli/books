!> @file convergence.f90
!!
!! Output of the convergence history.
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

!> Monitors the convergence, prints it out and stores it in a file. For external
!! flow, it also prints out the lift, the drag and the moment coefficients. For
!! internal flow, it prints out the mass flow and the mass flow ratio.
!!
subroutine Convergence

  use ModDataTypes
  use ModFiles
  use ModControl
  use ModGeometry
  use ModNumerics
  use ModPhysics
  use ModPlotQuant
  use ModInterfaces, only : Forces, Massflow
  implicit none

! local variables
  integer     :: i, j, idr, jdr
  real(rtype) :: dr, drmax

! *****************************************************************************
! compute the residual

  drho  = 0.D0
  drmax = 0.D0
  do j=2,j2
    do i=2,i2
      dr   = cv(1,i,j) - cvold(1,i,j)
      drho = drho + dr*dr
      if (Abs(dr) >= drmax) then
        drmax = Abs(dr)
        idr   = i
        jdr   = j
      endif
    enddo
  enddo

  if (iter == 1) then
    drho1 = Sqrt(drho) + 1.D-32
    drho  = 1.D0
  else
    drho  = Sqrt(drho)/drho1
  endif

! compute forces & moments (external flow)

  if (kflow == "E") then
    call Forces

! compute mass flow and mass flow ratio (internal flow)

  else
    call Massflow
  endif

! print out / store

  if (kflow == "E") then
    write(ifConv,1000) iter,Log10(drho),drmax,idr,jdr,cl,cd,cm
    write(*,1005) iter,Log10(drho),drmax,idr,jdr,cl,cd,cm
  else
    write(ifConv,1010) iter,Log10(drho),drmax,idr,jdr,mflow,mfratio
    write(*,1015) iter,Log10(drho),drmax,idr,jdr,mflow,mfratio
  endif

1000  format(I5,1P,2X,E12.5,2X,E12.5,0P,2I7,1P,3(2X,E12.5))
1005  format(I5,1P,2X,E11.4,2X,E10.4,0P,2I7,1P,3(2X,E10.3))
1010  format(I5,1P,2X,E12.5,2X,E12.5,0P,2I7,1X,1P,2(2X,E12.5))
1015  format(I5,1P,2X,E11.4,2X,E10.4,0P,2I7,1X,1P,2(2X,E10.4))

end subroutine Convergence

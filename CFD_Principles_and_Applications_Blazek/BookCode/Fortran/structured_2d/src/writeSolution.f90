!> @file writeSolution.f90
!!
!! Output of flow solution (in binary format).
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: March 6, 2014
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

!> Stores the current flow solution together with the initial residual
!! and the actual number of iterations.
!!
subroutine WriteSolution

  use ModControl
  use ModFiles
  use ModGeometry
  use ModPhysics
  use ModPlotQuant
  use ModInterfaces, only : ErrorMessage
  implicit none

! local variables
  integer :: errFlag, i, j, n

! *****************************************************************************

  open(unit=ifRsto, file=fnRsto, status="unknown", action="write", &
       form="unformatted", iostat=errFlag)
  if (errFlag /= 0) call ErrorMessage( "cannot open solution file" )

! dimensions

  write(ifRsto) imax,jmax,nconv

! initial residual, iteration # and solution

  write(ifRsto) drho1,iter
  write(ifRsto) (((cv(n,i,j), i=0,imax), j=0,jmax), n=1,nconv)

  close(ifRsto)

end subroutine WriteSolution

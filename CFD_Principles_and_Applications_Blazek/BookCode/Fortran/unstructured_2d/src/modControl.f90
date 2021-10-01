!> @file modControl.f90
!!
!! Variables controlling execution of the program.
!
! *****************************************************************************
!
!  (c) Jiri Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: February 25, 2014
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

!> Variables related to program control.
!!
module ModControl

  use ModDataTypes
  implicit none

  character(1) :: lrest !< use of previous solution for restart ("Y"=yes, "N"=no)

  integer :: maxiter, & !< max. number of iterations
             outstep, & !< number of iterations between solution dumps
             iter       !< actual iteration number

  real(rtype) :: convtol !< convergence criterion (2-norm of density change for
                         !! which the iteration process is stopped)

end module ModControl

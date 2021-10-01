!> @file modFiles.f90
!!
!! Names of files and numbers of I/O channels.
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

!> Variables and parameters related to file I/O.
!!
module ModFiles

  use ModDataTypes
  implicit none

  character(chrlen) :: fnGrid, & !< grid data
                       fnGtop, & !< grid topology and boundary conditions
                       fnFlow, & !< flow field (+ 5 digit iteration number + .v2d)
                       fnSurf, & !< quantities along wall surface(s) (+ 5 digit iteration number + .v2d)
                       fnConv, & !< convergence history (+ .v2d)
                       fnRsti, & !< restart solution - input
                       fnRsto    !< restart solution - output

  integer, parameter :: ifInp  = 10, & !< user input file (name stored in main.f90)
                        ifGrid = 20, &
                        ifGtop = 30, &
                        ifFlow = 40, &
                        ifSurf = 50, &
                        ifConv = 60, &
                        ifRsti = 70, &
                        ifRsto = 80

end module ModFiles

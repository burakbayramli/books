!> @file readChar.f90
!!
!! Extraction of single-character user option from input file.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: March 5, 2014
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

!> Reads in a single-character user option from file (in ASCII format).
!!
!! @param iunit input-file unit
!! @return option as single letter
!!
function ReadChar( iunit )

  use ModDataTypes
  implicit none

! parameters
  integer, intent(in) :: iunit

! result
  character(1) :: ReadChar

! local variables
  character(chrlen) :: str
  integer :: i

! *****************************************************************************

  read(iunit,"(A)") str

  do i=1,Len_trim(str)
    ReadChar = str(i:i)
    if (ReadChar /= " ") exit  ! first non-empty char should be the option ...
  enddo

end function ReadChar


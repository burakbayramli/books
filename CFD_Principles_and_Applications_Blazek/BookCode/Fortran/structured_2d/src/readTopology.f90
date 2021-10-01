!> @file readTopology.f90
!!
!! Input of grid topology and boundaries.
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

!> Reads in grid topology and types of boundary conditions.
!!
subroutine ReadTopology

  use ModFiles
  use ModGeometry
  use ModInterfaces, only : ErrorMessage
  implicit none

! local variables
  integer :: errFlag, iseg, ientry

! *****************************************************************************

  open(unit=ifGtop, file=fnGtop, status="old", action="read", iostat=errFlag)
  if (errFlag /= 0) call ErrorMessage( "cannot open topology file" )

! header, no. of segments

  read(ifGtop,"(1X)")
  read(ifGtop,"(1X)")
  read(ifGtop,"(1X)")

  read(ifGtop,*) nsegs
  allocate( lbsegs(nsegs,7),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for lbsegs()" )

! grid dimensions (no. of cells)

  read(ifGtop,"(1X)")
  read(ifGtop,"(1X)")
  read(ifGtop,*) nci,ncj

  i2   = nci + 1
  il   = nci + 2
  imax = nci + 3
  j2   = ncj + 1
  jl   = ncj + 2
  jmax = ncj + 3

! segments

  read(ifGtop,"(1X)")
  read(ifGtop,"(1X)")
  read(ifGtop,"(1X)")
  read(ifGtop,"(1X)")
  read(ifGtop,"(1X)")

  do iseg=1,nsegs
    read(ifGtop,"(1X)")
    read(ifGtop,*) (lbsegs(iseg,ientry), ientry=1,7)
  enddo

  close(unit=ifGtop)

end subroutine ReadTopology

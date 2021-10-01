!> @file volumeProjections.f90
!!
!! Computation of control volume projections.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: June 2, 2014
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

!> Computes projections of the control volumes on the x- and y-axis.
!! Variable (sproj) is used later to compute the time step.
!!
subroutine VolumeProjections

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModInterfaces, only : ErrorMessage
  implicit none

! local variables
  integer     :: errFlag, ibegn, iendn, ibegf, iendf
  integer     :: i, j, ib, ibf, ibn, ie
  real(rtype) :: sx, sy

! *****************************************************************************
! allocate memory

  allocate( sproj(2,nnodes),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for sproj()" )

! zero out the variable

  do i=1,nnodes
    sproj(1,i) = 0.D0
    sproj(2,i) = 0.D0
  enddo

! sum up contributions from interior edges

  do ie=1,nedint
    i          = edge(1,ie)
    j          = edge(2,ie)
    sx         = 0.5D0*Abs(sij(1,ie))
    sy         = 0.5D0*Abs(sij(2,ie))
    sproj(1,i) = sproj(1,i) + sx
    sproj(2,i) = sproj(2,i) + sy
    sproj(1,j) = sproj(1,j) + sx
    sproj(2,j) = sproj(2,j) + sy
  enddo

! add contributions from boundaries (except periodic)

  ibegf = 1
  do ib=1,nsegs
    iendf = ibound(1,ib)
    if (btype(ib)<700 .or. btype(ib)>=800) then
      do ibf=ibegf,iendf       ! loop over boundary faces
        i          = bface(1,ibf)
        j          = bface(2,ibf)
        sx         = 0.25D0*Abs(sbf(1,ibf))
        sy         = 0.25D0*Abs(sbf(2,ibf))
        sproj(1,i) = sproj(1,i) + sx
        sproj(2,i) = sproj(2,i) + sy
        sproj(1,j) = sproj(1,j) + sx
        sproj(2,j) = sproj(2,j) + sy
      enddo
    endif
    ibegf = iendf + 1
  enddo

! sum up at periodic boundaries

  ibegn = 1
  do ib=1,nsegs
    iendn = ibound(2,ib)
    if (btype(ib)>=700 .and. btype(ib)<800) then
      do ibn=ibegn,iendn
        i          = bnode(1,ibn)
        j          = bnode(2,ibn)
        sproj(1,i) = sproj(1,i) + sproj(1,j)
        sproj(1,j) = sproj(1,i)
        sproj(2,i) = sproj(2,i) + sproj(2,j)
        sproj(2,j) = sproj(2,i)
      enddo
    endif
    ibegn = iendn + 1
  enddo

end subroutine VolumeProjections

!> @file fluxRoe1.f90
!!
!! Computation of the convective fluxes based on flux averages.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: March 25, 2014
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

!> Computes the convective fluxes using average of fluxes at cell faces.
!! The left and right fluxes are computed directly from values at the cells
!! "I-1" and "I". This represents 1st-order accurate Roe scheme. Fluxes across
!! grid boundaries are evaluated in a second step.
!!
subroutine FluxRoe1

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  use ModInterfaces, only : FluxBoundary, FluxRoeBound
  implicit none

! local variables
  integer     :: itype, lb, lbeg, lend
  integer     :: i, j, iseg
  real(rtype) :: rhl, rhr, qsl, qsr, pav
  real(rtype) :: fc(4)

! *****************************************************************************
! initialize residual by adding upwind dissipation

  do j=2,j2
    do i=2,i2
      rhs(1,i,j) = -diss(1,i,j)
      rhs(2,i,j) = -diss(2,i,j)
      rhs(3,i,j) = -diss(3,i,j)
      rhs(4,i,j) = -diss(4,i,j)
    enddo
  enddo

! fluxes across interior cell faces -------------------------------------------

  do j=2,j2

!- flux in i-direction (except through boundary)

    do i=3,i2
      rhl =  dv(1,i-1,j) + cv(4,i-1,j)
      qsl = (cv(2,i-1,j)*si(1,i,j)+cv(3,i-1,j)*si(2,i,j))/cv(1,i-1,j)
      rhr =  dv(1,i  ,j) + cv(4,i  ,j)
      qsr = (cv(2,i  ,j)*si(1,i,j)+cv(3,i  ,j)*si(2,i,j))/cv(1,i  ,j)

      pav   = 0.5D0*(dv(1,i-1,j)+dv(1,i,j))
      fc(1) = 0.5D0*(qsl*cv(1,i-1,j)+qsr*cv(1,i,j))
      fc(2) = 0.5D0*(qsl*cv(2,i-1,j)+qsr*cv(2,i,j)) + si(1,i,j)*pav
      fc(3) = 0.5D0*(qsl*cv(3,i-1,j)+qsr*cv(3,i,j)) + si(2,i,j)*pav
      fc(4) = 0.5D0*(qsl*rhl        +qsr*rhr)

      rhs(1,i  ,j) = rhs(1,i  ,j) + fc(1)
      rhs(2,i  ,j) = rhs(2,i  ,j) + fc(2)
      rhs(3,i  ,j) = rhs(3,i  ,j) + fc(3)
      rhs(4,i  ,j) = rhs(4,i  ,j) + fc(4)

      rhs(1,i-1,j) = rhs(1,i-1,j) - fc(1)
      rhs(2,i-1,j) = rhs(2,i-1,j) - fc(2)
      rhs(3,i-1,j) = rhs(3,i-1,j) - fc(3)
      rhs(4,i-1,j) = rhs(4,i-1,j) - fc(4)
    enddo

! - flux in j-direction (except through boundary)

    if (j > 2) then
      do i=2,i2
        rhl =  dv(1,i,j-1) + cv(4,i,j-1)
        qsl = (cv(2,i,j-1)*sj(1,i,j)+cv(3,i,j-1)*sj(2,i,j))/cv(1,i,j-1)
        rhr =  dv(1,i,j  ) + cv(4,i,j  )
        qsr = (cv(2,i,j  )*sj(1,i,j)+cv(3,i,j  )*sj(2,i,j))/cv(1,i,j  )

        pav   = 0.5D0*(dv(1,i,j-1)+dv(1,i,j))
        fc(1) = 0.5D0*(qsl*cv(1,i,j-1)+qsr*cv(1,i,j))
        fc(2) = 0.5D0*(qsl*cv(2,i,j-1)+qsr*cv(2,i,j)) + sj(1,i,j)*pav
        fc(3) = 0.5D0*(qsl*cv(3,i,j-1)+qsr*cv(3,i,j)) + sj(2,i,j)*pav
        fc(4) = 0.5D0*(qsl*rhl        +qsr*rhr)

        rhs(1,i,j  ) = rhs(1,i,j  ) + fc(1)
        rhs(2,i,j  ) = rhs(2,i,j  ) + fc(2)
        rhs(3,i,j  ) = rhs(3,i,j  ) + fc(3)
        rhs(4,i,j  ) = rhs(4,i,j  ) + fc(4)

        rhs(1,i,j-1) = rhs(1,i,j-1) - fc(1)
        rhs(2,i,j-1) = rhs(2,i,j-1) - fc(2)
        rhs(3,i,j-1) = rhs(3,i,j-1) - fc(3)
        rhs(4,i,j-1) = rhs(4,i,j-1) - fc(4)
      enddo
    endif

  enddo ! j=2,j2

! boundary treatment ----------------------------------------------------------

  do iseg=1,nsegs
    itype = lbsegs(iseg,1)
    lb    = lbsegs(iseg,2)
    lbeg  = lbsegs(iseg,3)
    lend  = lbsegs(iseg,4)
    call FluxBoundary( lb,lbeg,lend,itype )
    call FluxRoeBound( lb,lbeg,lend,itype )
  enddo

end subroutine FluxRoe1

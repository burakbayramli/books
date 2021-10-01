!> @file fluxCentral.f90
!!
!! Computation of the convective fluxes based on central averages.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: March 17, 2014
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

!> Computes convective fluxes using the average of variables at cell faces.
!! Fluxes across grid boundaries are evaluated in a second step.
!!
subroutine FluxCentral

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  use ModInterfaces, only : FluxBoundary, FluxCentralBound
  implicit none

! local variables
  integer     :: i, j, iseg, itype, lb, lbeg, lend
  real(rtype) :: rhoa, rhoua, rhova, rhoea, pa, vcont
  real(rtype) :: fc(4)

! *****************************************************************************
! initialize residual by adding artificial dissipation

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
      rhoa  = 0.5D0*(cv(1,i-1,j)+cv(1,i,j))
      rhoua = 0.5D0*(cv(2,i-1,j)+cv(2,i,j))
      rhova = 0.5D0*(cv(3,i-1,j)+cv(3,i,j))
      rhoea = 0.5D0*(cv(4,i-1,j)+cv(4,i,j))
      pa    = 0.5D0*(dv(1,i-1,j)+dv(1,i,j))
      vcont = (rhoua*si(1,i,j)+rhova*si(2,i,j))/rhoa

      fc(1) = vcont*rhoa
      fc(2) = vcont*rhoua + pa*si(1,i,j)
      fc(3) = vcont*rhova + pa*si(2,i,j)
      fc(4) = vcont*(rhoea+pa)

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
        rhoa  = 0.5D0*(cv(1,i,j-1)+cv(1,i,j))
        rhoua = 0.5D0*(cv(2,i,j-1)+cv(2,i,j))
        rhova = 0.5D0*(cv(3,i,j-1)+cv(3,i,j))
        rhoea = 0.5D0*(cv(4,i,j-1)+cv(4,i,j))
        pa    = 0.5D0*(dv(1,i,j-1)+dv(1,i,j))
        vcont = (rhoua*sj(1,i,j)+rhova*sj(2,i,j))/rhoa

        fc(1) = vcont*rhoa
        fc(2) = vcont*rhoua + pa*sj(1,i,j)
        fc(3) = vcont*rhova + pa*sj(2,i,j)
        fc(4) = vcont*(rhoea+pa)

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
    call FluxCentralBound( lb,lbeg,lend,itype )
  enddo

end subroutine FluxCentral

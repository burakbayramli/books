!> @file initMetrics.f90
!!
!! Initialization of grid metrics.
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

!> Initializes grid metrics: computes face vectors and cell volumes.
!!
subroutine InitMetrics

  use ModDataTypes
  use ModGeometry
  use ModInterfaces, only : BcondCutSingle
  implicit none

! local variables
  integer     :: i, j, iseg
  integer     :: itype, lb, lbeg, lend, lbs, lbegs, lends
  real(rtype) :: sumx, sumy, sumNorm, smax, volMin, volMax

! *****************************************************************************
! face vectors: i-direction

  do j=0,jl
    do i=0,imax
      si(1,i,j) = y(i,j  ) - y(i,j+1)
      si(2,i,j) = x(i,j+1) - x(i,j  )
    enddo
  enddo

! face vectors: j-direction

  do j=0,jmax
    do i=0,il
      sj(1,i,j) = y(i+1,j) - y(i  ,j)
      sj(2,i,j) = x(i  ,j) - x(i+1,j)
    enddo
  enddo

! cell volumes

  do j=2,j2
    do i=2,i2
      vol(i,j) = 0.5D0*((x(i,j)-x(i+1,j+1))*(y(i+1,j)-y(i,j+1)) + &
                        (x(i,j+1)-x(i+1,j))*(y(i,j)-y(i+1,j+1)))
    enddo
  enddo

  do i=2,i2
    vol(i,   1) = vol(i, 2)
    vol(i,   0) = vol(i, 2)
    vol(i,  jl) = vol(i,j2)
    vol(i,jmax) = vol(i,j2)
  enddo
  do j=2,j2
    vol(   1,j) = vol( 2,j)
    vol(   0,j) = vol( 2,j)
    vol(  il,j) = vol(i2,j)
    vol(imax,j) = vol(i2,j)
  enddo
  do iseg=1,nsegs
    itype = lbsegs(iseg,1)
    lb    = lbsegs(iseg,2)
    lbeg  = lbsegs(iseg,3)
    lend  = lbsegs(iseg,4)
    if (itype>=700 .and. itype<800) then  ! coord. cut or period. boundary
      lbs   = lbsegs(iseg,5)
      lbegs = lbsegs(iseg,6)
      lends = lbsegs(iseg,7)
      call BcondCutSingle( lb,lbeg,lend,lbs,lbegs,lends,vol )
    endif
  enddo

! get sum of face vectors and min./max. volume

  smax   = -1.0D+32
  volMin = +1.0D+32
  volMax = -1.0D+32
  do j=2,j2
    do i=2,i2
      sumx    = si(1,i,j) - si(1,i+1,j) + sj(1,i,j) - sj(1,i,j+1)
      sumy    = si(2,i,j) - si(2,i+1,j) + sj(2,i,j) - sj(2,i,j+1)
      sumNorm = Sqrt(sumx*sumx+sumy*sumy)
      smax    = Max(sumNorm,smax)
      volMin  = Min(volMin,vol(i,j))
      volMax  = Max(volMax,vol(i,j))
    enddo
  enddo

  write(*,1000) smax,volMin,volMax

1000  format('   max. sum(S) = ',E11.4,/,'   min. volume = ',E11.4,/, &
             '   max. volume = ',E11.4,/)

end subroutine InitMetrics

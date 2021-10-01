!> @file limiterInit.f90
!!
!! Computation of min./max. values around a node.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: June 4, 2014
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

!> Computes minimum and maximum values of density, u, v, and pressure for
!! all direct neighbors "j" of node "i" (min/max_j U_j in Eq. (5.61)).
!! This is used later in Limiter to evaluate the limiter functions.
!!
!! @param umin  minimum of U_i and of min_j U_j (U = rho, u, v, p)
!! @param umax  maximum of U_i and of max_j U_j (U = rho, u, v, p)
!!
subroutine LimiterInit( umin,umax )

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  implicit none

! parameters
  real(rtype), intent(out) :: umin(:,:), umax(:,:)

! local variables
  integer     :: i, j, ib, ibn, ie, ibegn, iendn
  real(rtype) :: rl, ul, vl, pl, rr, ur, vr, pr

! *****************************************************************************
! initialize with values at node i

  do i=1,nndint
    umin(1,i) = cv(1,i)
    umin(2,i) = cv(2,i)/cv(1,i)
    umin(3,i) = cv(3,i)/cv(1,i)
    umin(4,i) = dv(1,i)

    umax(1,i) = umin(1,i)
    umax(2,i) = umin(2,i)
    umax(3,i) = umin(3,i)
    umax(4,i) = umin(4,i)
  enddo

! loop over interior edges

  do ie=1,nedint
    i = edge(1,ie)
    j = edge(2,ie)

! - left state

    rl = cv(1,i)
    ul = cv(2,i)/rl
    vl = cv(3,i)/rl
    pl = dv(1,i)

! - right state

    rr = cv(1,j)
    ur = cv(2,j)/rr
    vr = cv(3,j)/rr
    pr = dv(1,j)

! - neighbors of node i

    umin(1,i) = Min(umin(1,i),rr)
    umin(2,i) = Min(umin(2,i),ur)
    umin(3,i) = Min(umin(3,i),vr)
    umin(4,i) = Min(umin(4,i),pr)

    umax(1,i) = Max(umax(1,i),rr)
    umax(2,i) = Max(umax(2,i),ur)
    umax(3,i) = Max(umax(3,i),vr)
    umax(4,i) = Max(umax(4,i),pr)

! - neighbors of node j

    umin(1,j) = Min(umin(1,j),rl)
    umin(2,j) = Min(umin(2,j),ul)
    umin(3,j) = Min(umin(3,j),vl)
    umin(4,j) = Min(umin(4,j),pl)

    umax(1,j) = Max(umax(1,j),rl)
    umax(2,j) = Max(umax(2,j),ul)
    umax(3,j) = Max(umax(3,j),vl)
    umax(4,j) = Max(umax(4,j),pl)
  enddo

! periodic boundaries

  ibegn = 1

  do ib=1,nsegs
    iendn = ibound(2,ib)
    if (btype(ib)>=700 .and. btype(ib)<800) then
      do ibn=ibegn,iendn
        i = bnode(1,ibn)
        j = bnode(2,ibn)

        umin(1,i) = Min(umin(1,i),umin(1,j))
        umin(2,i) = Min(umin(2,i),umin(2,j))
        umin(3,i) = Min(umin(3,i),umin(3,j))
        umin(4,i) = Min(umin(4,i),umin(4,j))

        umax(1,i) = Max(umax(1,i),umax(1,j))
        umax(2,i) = Max(umax(2,i),umax(2,j))
        umax(3,i) = Max(umax(3,i),umax(3,j))
        umax(4,i) = Max(umax(4,i),umax(4,j))

        umin(1,j) = umin(1,i)
        umin(2,j) = umin(2,i)
        umin(3,j) = umin(3,i)
        umin(4,j) = umin(4,i)

        umax(1,j) = umax(1,i)
        umax(2,j) = umax(2,i)
        umax(3,j) = umax(3,i)
        umax(4,j) = umax(4,i)
      enddo
    endif
    ibegn = iendn + 1
  enddo

end subroutine LimiterInit

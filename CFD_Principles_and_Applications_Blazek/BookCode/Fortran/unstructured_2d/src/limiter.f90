!> @file limiter.f90
!!
!! Computation of limiter functions.
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

!> Evaluates limiter functions (Venkatakrishnan's limiter, Eq. (5.67)).
!!
!! @param umin  minimum of U_i and of min_j U_j (U = rho, u, v, p)
!! @param umax  maximum of U_i and of max_j U_j (U = rho, u, v, p)
!!
subroutine Limiter( umin,umax )

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  implicit none

! parameters
  real(rtype), intent(in) :: umin(:,:), umax(:,:)

! local variables
  integer     :: i, j, ib, ibn, ie, ibegn, iendn
  real(rtype) :: limfac3, rvolref, rx, ry, voll, eps2nl, d1minl, d1maxl, &
                 d2l, ul, vl, volr, eps2nr, d1minr, d1maxr, d2r, ur, vr, &
                 limval
  real(rtype) :: eps2(4)

! *****************************************************************************
! initialize limiter functions (1.0 = no limiting)

  do i=1,nnodes
    lim(1,i) = 1.D0
    lim(2,i) = 1.D0
    lim(3,i) = 1.D0
    lim(4,i) = 1.D0
  enddo

! normalize epsilon^2 for all limited variables (rho, u, v, p)

  limfac3 = limfac*limfac*limfac
  rvolref = 1.D0/volref**1.5D0
  eps2(1) = limfac3*limref(1)*limref(1)*rvolref
  eps2(2) = limfac3*limref(2)*limref(2)*rvolref
  eps2(3) = limfac3*limref(3)*limref(3)*rvolref
  eps2(4) = limfac3*limref(4)*limref(4)*rvolref

! evaluate limiter functions --------------------------------------------------

  do ie=1,nedint
    i = edge(1,ie)
    j = edge(2,ie)

    rx   = 0.5D0*(x(j)-x(i))
    ry   = 0.5D0*(y(j)-y(i))
    voll = vol(i)**1.5D0
    volr = vol(j)**1.5D0

! - density

    eps2nl   = eps2(1)*voll
    d1minl   = umin(1,i) - cv(1,i)
    d1maxl   = umax(1,i) - cv(1,i)
    eps2nr   = eps2(1)*volr
    d1minr   = umin(1,j) - cv(1,j)
    d1maxr   = umax(1,j) - cv(1,j)
    d2l      =  gradx(1,i)*rx + grady(1,i)*ry
    d2r      = -gradx(1,j)*rx - grady(1,j)*ry
    limval   = Venkat( d2l,d1minl,d1maxl,eps2nl )
    lim(1,i) = Min(limval,lim(1,i))
    limval   = Venkat( d2r,d1minr,d1maxr,eps2nr )
    lim(1,j) = Min(limval,lim(1,j))

! - u

    ul       = cv(2,i)/cv(1,i)
    ur       = cv(2,j)/cv(1,j)
    eps2nl   = eps2(2)*voll
    d1minl   = umin(2,i) - ul
    d1maxl   = umax(2,i) - ul
    eps2nr   = eps2(2)*volr
    d1minr   = umin(2,j) - ur
    d1maxr   = umax(2,j) - ur
    d2l      =  gradx(2,i)*rx + grady(2,i)*ry
    d2r      = -gradx(2,j)*rx - grady(2,j)*ry
    limval   = Venkat( d2l,d1minl,d1maxl,eps2nl )
    lim(2,i) = Min(limval,lim(2,i))
    limval   = Venkat( d2r,d1minr,d1maxr,eps2nr )
    lim(2,j) = Min(limval,lim(2,j))

! - v

    vl       = cv(3,i)/cv(1,i)
    vr       = cv(3,j)/cv(1,j)
    eps2nl   = eps2(3)*voll
    d1minl   = umin(3,i) - vl
    d1maxl   = umax(3,i) - vl
    eps2nr   = eps2(3)*volr
    d1minr   = umin(3,j) - vr
    d1maxr   = umax(3,j) - vr
    d2l      =  gradx(3,i)*rx + grady(3,i)*ry
    d2r      = -gradx(3,j)*rx - grady(3,j)*ry
    limval   = Venkat( d2l,d1minl,d1maxl,eps2nl )
    lim(3,i) = Min(limval,lim(3,i))
    limval   = Venkat( d2r,d1minr,d1maxr,eps2nr )
    lim(3,j) = Min(limval,lim(3,j))

! - pressure

    eps2nl   = eps2(4)*voll
    d1minl   = umin(4,i) - dv(1,i)
    d1maxl   = umax(4,i) - dv(1,i)
    eps2nr   = eps2(4)*volr
    d1minr   = umin(4,j) - dv(1,j)
    d1maxr   = umax(4,j) - dv(1,j)
    d2l      =  gradx(4,i)*rx + grady(4,i)*ry
    d2r      = -gradx(4,j)*rx - grady(4,j)*ry
    limval   = Venkat( d2l,d1minl,d1maxl,eps2nl )
    lim(4,i) = Min(limval,lim(4,i))
    limval   = Venkat( d2r,d1minr,d1maxr,eps2nr )
    lim(4,j) = Min(limval,lim(4,j))
  enddo

! periodic boundaries ---------------------------------------------------------

  ibegn = 1

  do ib=1,nsegs
    iendn = ibound(2,ib)
    if (btype(ib)>=700 .and. btype(ib)<800) then
      do ibn=ibegn,iendn
        i = bnode(1,ibn)
        j = bnode(2,ibn)

        lim(1,i) = Min(lim(1,i),lim(1,j))
        lim(2,i) = Min(lim(2,i),lim(2,j))
        lim(3,i) = Min(lim(3,i),lim(3,j))
        lim(4,i) = Min(lim(4,i),lim(4,j))

        lim(1,j) = lim(1,i)
        lim(2,j) = lim(2,i)
        lim(3,j) = lim(3,i)
        lim(4,j) = lim(4,i)
      enddo
    endif
    ibegn = iendn + 1
  enddo

! *****************************************************************************

contains

!> Evaluates the limiter function
!!
!! @param d2     change of value to be limited (gradient * distance)
!! @param d1min  min(U_i, U_j) - U_i
!! @param d1max  max(U_i, U_j) - U_i
!! @param eps2   threshold value
!! @return       limiter value
!!
  real(rtype) function Venkat( d2,d1min,d1max,eps2 )
    implicit none
    real(rtype) :: d2, d1min, d1max, eps2
    real(rtype) :: num, den

    Venkat = 1.D0
    if (d2 > 1.D-12) then
      num    = (d1max*d1max+eps2)*d2 + 2.D0*d2*d2*d1max
      den    = d2*(d1max*d1max+2.D0*d2*d2+d1max*d2+eps2)
      Venkat = num/den
    else if (d2 < -1.D-12) then
      num    = (d1min*d1min+eps2)*d2 + 2.D0*d2*d2*d1min
      den    = d2*(d1min*d1min+2.D0*d2*d2+d1min*d2+eps2)
      Venkat = num/den
    endif
  end function Venkat

end subroutine Limiter

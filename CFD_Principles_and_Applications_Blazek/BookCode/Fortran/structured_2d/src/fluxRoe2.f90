!> @file fluxRoe2.f90
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
!! The left and right fluxes are computed from interpolated and limited
!! values at the cells "I-1" and "I" (MUSCL scheme). Fluxes across grid
!! boundaries are evaluated in a second step.
!!
subroutine FluxRoe2

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  use ModInterfaces, only : FluxBoundary, FluxRoeBound
  implicit none

! local variables
  integer     :: im1, jm1, ip1, jp1, itype, lb, lbeg, lend
  integer     :: i, j, iseg
  real(rtype) :: limfac3, rvolref, rrho, gam1, ggm1, rl, ul, vl, pl, hl, &
                 rr, ur, vr, pr, hr, qsrl, qsrr, pav, vola, eps2n
  real(rtype) :: eps2(4), deltl(4), deltr(4), fc(4)

! functions
  real(rtype) :: MUSCL3
  real(rtype) :: af, bf, eps

! *****************************************************************************
! limiter function:
! MUSCL3 = MUSCL scheme with kappa=1/3 (Eq. (4.117))

  MUSCL3(af,bf,eps) = (bf*(2.D0*af*af+eps)+af*(bf*bf+2.D0*eps))/ &
                      (2.D0*af*af+2.D0*bf*bf-af*bf+3.D0*eps+1.D-30)

! normalize epsilon^2 for all limited variables (rho, u, v, p)

  limfac3 = limfac*limfac*limfac
  rvolref = 1.D0/volref**1.5D0
  eps2(1) = limfac3*limref(1)*limref(1)*rvolref
  eps2(2) = limfac3*limref(2)*limref(2)*rvolref
  eps2(3) = limfac3*limref(3)*limref(3)*rvolref
  eps2(4) = limfac3*limref(4)*limref(4)*rvolref

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
      im1  = i - 1
      ip1  = i + 1
      vola = (0.5D0*(vol(im1,j)+vol(i,j)))**1.5D0

! --- limited extrapolations

      eps2n    = eps2(1)*vola
      deltl(1) = 0.5D0*MUSCL3( dui(1,i  ,j),dui(1,im1,j),eps2n )
      deltr(1) = 0.5D0*MUSCL3( dui(1,ip1,j),dui(1,i  ,j),eps2n )
      eps2n    = eps2(2)*vola
      deltl(2) = 0.5D0*MUSCL3( dui(2,i  ,j),dui(2,im1,j),eps2n )
      deltr(2) = 0.5D0*MUSCL3( dui(2,ip1,j),dui(2,i  ,j),eps2n )
      eps2n    = eps2(3)*vola
      deltl(3) = 0.5D0*MUSCL3( dui(3,i  ,j),dui(3,im1,j),eps2n )
      deltr(3) = 0.5D0*MUSCL3( dui(3,ip1,j),dui(3,i  ,j),eps2n )
      eps2n    = eps2(4)*vola
      deltl(4) = 0.5D0*MUSCL3( dui(4,i  ,j),dui(4,im1,j),eps2n )
      deltr(4) = 0.5D0*MUSCL3( dui(4,ip1,j),dui(4,i  ,j),eps2n )

! --- left and right state (at I-1/2)

      rrho = 1.D0/cv(1,im1,j)
      gam1 = dv(4,im1,j) - 1.D0
      ggm1 = dv(4,im1,j)/gam1
      rl   = cv(1,im1,j)      + deltl(1)
      ul   = cv(2,im1,j)*rrho + deltl(2)
      vl   = cv(3,im1,j)*rrho + deltl(3)
      pl   = dv(1,im1,j)      + deltl(4)
      hl   = ggm1*pl/rl + 0.5D0*(ul*ul+vl*vl)
      qsrl = (ul*si(1,i,j)+vl*si(2,i,j))*rl

      rrho = 1.D0/cv(1,i,j)
      gam1 = dv(4,i,j) - 1.D0
      ggm1 = dv(4,i,j)/gam1
      rr   = cv(1,i,j)      - deltr(1)
      ur   = cv(2,i,j)*rrho - deltr(2)
      vr   = cv(3,i,j)*rrho - deltr(3)
      pr   = dv(1,i,j)      - deltr(4)
      hr   = ggm1*pr/rr + 0.5D0*(ur*ur+vr*vr)
      qsrr = (ur*si(1,i,j)+vr*si(2,i,j))*rr

! --- fluxes

      pav   = 0.5D0*(pl+pr)
      fc(1) = 0.5D0*(qsrl   +qsrr   )
      fc(2) = 0.5D0*(qsrl*ul+qsrr*ur) + si(1,i,j)*pav
      fc(3) = 0.5D0*(qsrl*vl+qsrr*vr) + si(2,i,j)*pav
      fc(4) = 0.5D0*(qsrl*hl+qsrr*hr)

      rhs(1,i  ,j) = rhs(1,i  ,j) + fc(1)
      rhs(2,i  ,j) = rhs(2,i  ,j) + fc(2)
      rhs(3,i  ,j) = rhs(3,i  ,j) + fc(3)
      rhs(4,i  ,j) = rhs(4,i  ,j) + fc(4)

      rhs(1,im1,j) = rhs(1,im1,j) - fc(1)
      rhs(2,im1,j) = rhs(2,im1,j) - fc(2)
      rhs(3,im1,j) = rhs(3,im1,j) - fc(3)
      rhs(4,im1,j) = rhs(4,im1,j) - fc(4)
    enddo

! - flux in j-direction (except through boundary)

    if (j > 2) then
      jm1 = j - 1
      jp1 = j + 1

      do i=2,i2
        vola = (0.5D0*(vol(i,jm1)+vol(i,j)))**1.5D0

! ----- limited extrapolations

        eps2n    = eps2(1)*vola
        deltl(1) = 0.5D0*MUSCL3( duj(1,i,j  ),duj(1,i,jm1),eps2n )
        deltr(1) = 0.5D0*MUSCL3( duj(1,i,jp1),duj(1,i,j  ),eps2n )
        eps2n    = eps2(2)*vola
        deltl(2) = 0.5D0*MUSCL3( duj(2,i,j  ),duj(2,i,jm1),eps2n )
        deltr(2) = 0.5D0*MUSCL3( duj(2,i,jp1),duj(2,i,j  ),eps2n )
        eps2n    = eps2(3)*vola
        deltl(3) = 0.5D0*MUSCL3( duj(3,i,j  ),duj(3,i,jm1),eps2n )
        deltr(3) = 0.5D0*MUSCL3( duj(3,i,jp1),duj(3,i,j  ),eps2n )
        eps2n    = eps2(4)*vola
        deltl(4) = 0.5D0*MUSCL3( duj(4,i,j  ),duj(4,i,jm1),eps2n )
        deltr(4) = 0.5D0*MUSCL3( duj(4,i,jp1),duj(4,i,j  ),eps2n )

! ----- left and right state (at J-1/2)

        rrho = 1.D0/cv(1,i,jm1)
        gam1 = dv(4,i,jm1) - 1.D0
        ggm1 = dv(4,i,jm1)/gam1
        rl   = cv(1,i,jm1)      + deltl(1)
        ul   = cv(2,i,jm1)*rrho + deltl(2)
        vl   = cv(3,i,jm1)*rrho + deltl(3)
        pl   = dv(1,i,jm1)      + deltl(4)
        hl   = ggm1*pl/rl + 0.5D0*(ul*ul+vl*vl)
        qsrl = (ul*sj(1,i,j)+vl*sj(2,i,j))*rl

        rrho = 1.D0/cv(1,i,j)
        gam1 = dv(4,i,j) - 1.D0
        ggm1 = dv(4,i,j)/gam1
        rr   = cv(1,i,j)      - deltr(1)
        ur   = cv(2,i,j)*rrho - deltr(2)
        vr   = cv(3,i,j)*rrho - deltr(3)
        pr   = dv(1,i,j)      - deltr(4)
        hr   = ggm1*pr/rr + 0.5D0*(ur*ur+vr*vr)
        qsrr = (ur*sj(1,i,j)+vr*sj(2,i,j))*rr

! ----- fluxes

        pav   = 0.5D0*(pl+pr)
        fc(1) = 0.5D0*(qsrl   +qsrr   )
        fc(2) = 0.5D0*(qsrl*ul+qsrr*ur) + sj(1,i,j)*pav
        fc(3) = 0.5D0*(qsrl*vl+qsrr*vr) + sj(2,i,j)*pav
        fc(4) = 0.5D0*(qsrl*hl+qsrr*hr)

        rhs(1,i,j  ) = rhs(1,i,j  ) + fc(1)
        rhs(2,i,j  ) = rhs(2,i,j  ) + fc(2)
        rhs(3,i,j  ) = rhs(3,i,j  ) + fc(3)
        rhs(4,i,j  ) = rhs(4,i,j  ) + fc(4)

        rhs(1,i,jm1) = rhs(1,i,jm1) - fc(1)
        rhs(2,i,jm1) = rhs(2,i,jm1) - fc(2)
        rhs(3,i,jm1) = rhs(3,i,jm1) - fc(3)
        rhs(4,i,jm1) = rhs(4,i,jm1) - fc(4)
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

end subroutine FluxRoe2

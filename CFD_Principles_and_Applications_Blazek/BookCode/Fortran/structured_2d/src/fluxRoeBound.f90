!> @file fluxRoeBound.f90
!!
!! Computation of upwind boundary fluxes.
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

!> Computes convective fluxes at boundaries using average of fluxes at cell
!! faces due to Roe's upwind scheme. Subroutine treats all boundaries except
!! solid walls, injection and symmetry planes.
!!
!! @param lb     side of the computational domain (1: j=2, 2: i=i2, 3: j=j2,4: i=2)
!! @param lbeg   start index of the boundary segment
!! @param lend   end index of the boundary segment
!! @param itype  type of boundary condition
!!
subroutine FluxRoeBound( lb,lbeg,lend,itype )

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  implicit none

! parameters
  integer, intent(in) :: lb, lbeg, lend, itype

! local variables
  integer     :: lstep, iins1, iins2, jins1, jins2, idum1, jdum1
  integer     :: i, j
  real(rtype) :: limfac3, rvolref, sx, sy, rrho, gam1, ggm1, rl, ul, vl, &
                 pl, hl, rr, ur, vr, pr, hr, qsrl, qsrr, pav, vola, eps2n
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

! set indexes

  if      (lb == 1) then
    jins1 = 2
    jins2 = 3
    jdum1 = 1
  else if (lb == 2) then
    iins1 = i2
    iins2 = i2 - 1
    idum1 = il
  else if (lb == 3) then
    jins1 = j2
    jins2 = j2 - 1
    jdum1 = jl
  else if (lb == 4) then
    iins1 = 2
    iins2 = 3
    idum1 = 1
  endif
                   lstep =  1
  if (lbeg > lend) lstep = -1

! boundary j=2 / j=j2 and i=lbeg,lend -----------------------------------------
! (flux in j-direction)

  if (lb==1 .or. lb==3) then

    if ((itype>=100 .and. itype<300) .or. &
        (itype>=600 .and. itype<800) .or. &
        (itype>=900)) then

      do i=lbeg,lend,lstep
        if (lb == 1) then
          sx =  sj(1,i,jins1)
          sy =  sj(2,i,jins1)
        else
          sx = -sj(1,i,jdum1)
          sy = -sj(2,i,jdum1)
        endif
        if (iorder < 2) then
          rrho = 1.D0/cv(1,i,jdum1)
          rl   = cv(1,i,jdum1)
          ul   = cv(2,i,jdum1)*rrho
          vl   = cv(3,i,jdum1)*rrho
          pl   = dv(1,i,jdum1)
          hl   = (pl+cv(4,i,jdum1))*rrho

          rrho = 1.D0/cv(1,i,jins1)
          rr   = cv(1,i,jins1)
          ur   = cv(2,i,jins1)*rrho
          vr   = cv(3,i,jins1)*rrho
          pr   = dv(1,i,jins1)
          hr   = (pr+cv(4,i,jins1))*rrho
        else
          vola     = (0.5D0*(vol(i,jdum1)+vol(i,jins1)))**1.5D0
          eps2n    = eps2(1)*vola
          deltl(1) = 0.5D0*MUSCL3( duj(1,i,jins1),duj(1,i,jdum1),eps2n )
          deltr(1) = 0.5D0*MUSCL3( duj(1,i,jins2),duj(1,i,jins1),eps2n )
          eps2n    = eps2(2)*vola
          deltl(2) = 0.5D0*MUSCL3( duj(2,i,jins1),duj(2,i,jdum1),eps2n )
          deltr(2) = 0.5D0*MUSCL3( duj(2,i,jins2),duj(2,i,jins1),eps2n )
          eps2n    = eps2(3)*vola
          deltl(3) = 0.5D0*MUSCL3( duj(3,i,jins1),duj(3,i,jdum1),eps2n )
          deltr(3) = 0.5D0*MUSCL3( duj(3,i,jins2),duj(3,i,jins1),eps2n )
          eps2n    = eps2(4)*vola
          deltl(4) = 0.5D0*MUSCL3( duj(4,i,jins1),duj(4,i,jdum1),eps2n )
          deltr(4) = 0.5D0*MUSCL3( duj(4,i,jins2),duj(4,i,jins1),eps2n )

          rrho = 1.D0/cv(1,i,jdum1)
          gam1 = dv(4,i,jdum1) - 1.D0
          ggm1 = dv(4,i,jdum1)/gam1
          rl   = cv(1,i,jdum1)      + deltl(1)
          ul   = cv(2,i,jdum1)*rrho + deltl(2)
          vl   = cv(3,i,jdum1)*rrho + deltl(3)
          pl   = dv(1,i,jdum1)      + deltl(4)
          hl   = ggm1*pl/rl + 0.5D0*(ul*ul+vl*vl)

          rrho = 1.D0/cv(1,i,jins1)
          gam1 = dv(4,i,jins1) - 1.D0
          ggm1 = dv(4,i,jins1)/gam1
          rr   = cv(1,i,jins1)      - deltr(1)
          ur   = cv(2,i,jins1)*rrho - deltr(2)
          vr   = cv(3,i,jins1)*rrho - deltr(3)
          pr   = dv(1,i,jins1)      - deltr(4)
          hr   = ggm1*pr/rr + 0.5D0*(ur*ur+vr*vr)
        endif
        qsrl  = (ul*sx+vl*sy)*rl
        qsrr  = (ur*sx+vr*sy)*rr
        pav   = 0.5D0*(pl+pr)
        fc(1) = 0.5D0*(qsrl   +qsrr   )
        fc(2) = 0.5D0*(qsrl*ul+qsrr*ur) + sx*pav
        fc(3) = 0.5D0*(qsrl*vl+qsrr*vr) + sy*pav
        fc(4) = 0.5D0*(qsrl*hl+qsrr*hr)

        rhs(1,i,jins1) = rhs(1,i,jins1) + fc(1)
        rhs(2,i,jins1) = rhs(2,i,jins1) + fc(2)
        rhs(3,i,jins1) = rhs(3,i,jins1) + fc(3)
        rhs(4,i,jins1) = rhs(4,i,jins1) + fc(4)
      enddo

    endif

! boundary i=2 / i=i2 and j=lbeg,lend -----------------------------------------
! (flux in i-direction)

  else if (lb==2 .or. lb==4) then

    if ((itype>=100 .and. itype<300) .or. &
        (itype>=600 .and. itype<800) .or. &
        (itype>=900)) then

      do j=lbeg,lend,lstep
        if (lb == 4) then
          sx =  si(1,iins1,j)
          sy =  si(2,iins1,j)
        else
          sx = -si(1,idum1,j)
          sy = -si(2,idum1,j)
        endif
        if (iorder < 2) then
          rrho = 1.D0/cv(1,idum1,j)
          rl   = cv(1,idum1,j)
          ul   = cv(2,idum1,j)*rrho
          vl   = cv(3,idum1,j)*rrho
          pl   = dv(1,idum1,j)
          hl   = (pl+cv(4,idum1,j))*rrho

          rrho = 1.D0/cv(1,iins1,j)
          rr   = cv(1,iins1,j)
          ur   = cv(2,iins1,j)*rrho
          vr   = cv(3,iins1,j)*rrho
          pr   = dv(1,iins1,j)
          hr   = (pr+cv(4,iins1,j))*rrho
        else
          vola     = (0.5D0*(vol(idum1,j)+vol(iins1,j)))**1.5D0
          eps2n    = eps2(1)*vola
          deltl(1) = 0.5D0*MUSCL3( dui(1,iins1,j),dui(1,idum1,j),eps2n )
          deltr(1) = 0.5D0*MUSCL3( dui(1,iins2,j),dui(1,iins1,j),eps2n )
          eps2n    = eps2(2)*vola
          deltl(2) = 0.5D0*MUSCL3( dui(2,iins1,j),dui(2,idum1,j),eps2n )
          deltr(2) = 0.5D0*MUSCL3( dui(2,iins2,j),dui(2,iins1,j),eps2n )
          eps2n    = eps2(3)*vola
          deltl(3) = 0.5D0*MUSCL3( dui(3,iins1,j),dui(3,idum1,j),eps2n )
          deltr(3) = 0.5D0*MUSCL3( dui(3,iins2,j),dui(3,iins1,j),eps2n )
          eps2n    = eps2(4)*vola
          deltl(4) = 0.5D0*MUSCL3( dui(4,iins1,j),dui(4,idum1,j),eps2n )
          deltr(4) = 0.5D0*MUSCL3( dui(4,iins2,j),dui(4,iins1,j),eps2n )

          rrho = 1.D0/cv(1,idum1,j)
          gam1 = dv(4,idum1,j) - 1.D0
          ggm1 = dv(4,idum1,j)/gam1
          rl   = cv(1,idum1,j)      + deltl(1)
          ul   = cv(2,idum1,j)*rrho + deltl(2)
          vl   = cv(3,idum1,j)*rrho + deltl(3)
          pl   = dv(1,idum1,j)      + deltl(4)
          hl   = ggm1*pl/rl + 0.5D0*(ul*ul+vl*vl)

          rrho = 1.D0/cv(1,iins1,j)
          gam1 = dv(4,iins1,j) - 1.D0
          ggm1 = dv(4,iins1,j)/gam1
          rr   = cv(1,iins1,j)      - deltr(1)
          ur   = cv(2,iins1,j)*rrho - deltr(2)
          vr   = cv(3,iins1,j)*rrho - deltr(3)
          pr   = dv(1,iins1,j)      - deltr(4)
          hr   = ggm1*pr/rr + 0.5D0*(ur*ur+vr*vr)
        endif
        qsrl  = (ul*sx+vl*sy)*rl
        qsrr  = (ur*sx+vr*sy)*rr
        pav   = 0.5D0*(pl+pr)
        fc(1) = 0.5D0*(qsrl   +qsrr   )
        fc(2) = 0.5D0*(qsrl*ul+qsrr*ur) + sx*pav
        fc(3) = 0.5D0*(qsrl*vl+qsrr*vr) + sy*pav
        fc(4) = 0.5D0*(qsrl*hl+qsrr*hr)

        rhs(1,iins1,j) = rhs(1,iins1,j) + fc(1)
        rhs(2,iins1,j) = rhs(2,iins1,j) + fc(2)
        rhs(3,iins1,j) = rhs(3,iins1,j) + fc(3)
        rhs(4,iins1,j) = rhs(4,iins1,j) + fc(4)
      enddo

    endif

  endif ! lb

end subroutine FluxRoeBound

!> @file timeStep.f90
!!
!! Computation of the time step.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: June 3, 2014
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

!> Computes maximum stable time step.
!!
subroutine TimeStep

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  use ModInterfaces, only : CompTheta
  implicit none

! local variables
  integer     :: i
  real(rtype) :: sx, sy, ds, rrho, u, v, vc, cs, rhop, rhoT, hT, q2, &
                 theta, ra1g, a1, a4, a5, fmue, f1, f2, fac, dtv, cfac, &
                 lambdac, lambdav, tsmin

! *****************************************************************************

  if (kequs == "E") then

! - preconditioned Euler equations

    if (kprecond == "Y") then
      do i=1,nndint
        sx       = sproj(1,i)
        sy       = sproj(2,i)
        ds       = sx + sy
        u        = Abs(cv(2,i)/cv(1,i))
        v        = Abs(cv(3,i)/cv(1,i))
        rhop     = cv(1,i)/dv(1,i)
        rhoT     = -cv(1,i)/dv(2,i)
        hT       = dv(5,i)
        q2       = u*u + v*v
        theta    = CompTheta( dv(4,i),dv(3,i),q2 )
        a1       = cv(1,i)*rhop*hT + rhoT
        ra1g     = 1.D0/(cv(1,i)*theta*hT+rhoT)
        a4       = a1*ra1g
        a5       = cv(1,i)*hT*ra1g
        vc       = (sx*u+sy*v)/ds
        cs       = Sqrt((vc*vc)*((a4-1.D0)*(a4-1.D0))+4.D0*a5)
        tstep(i) = (2.D0*vol(i))/((vc*(a4+1.D0)+cs)*ds)
      enddo

! - Euler equations

    else
      do i=1,nndint
        sx       = sproj(1,i)
        sy       = sproj(2,i)
        u        = Abs(cv(2,i)/cv(1,i))
        v        = Abs(cv(3,i)/cv(1,i))
        vc       = sx*u + sy*v
        cs       = dv(3,i)*(sx+sy)
        tstep(i) = vol(i)/(vc+cs)
      enddo
    endif

  else  ! kequs == "N"

    if (iorder > 1) then
      cfac = 1.D0
    else
      cfac = 2.D0
    endif

! - preconditioned Navier-Stokes equations (laminar)

    if (kprecond == "Y") then
      do i=1,nndint
        sx       = sproj(1,i)
        sy       = sproj(2,i)
        ds       = sx + sy
        rrho     = 1.D0/cv(1,i)
        u        = Abs(cv(2,i)*rrho)
        v        = Abs(cv(3,i)*rrho)
        rhop     = cv(1,i)/dv(1,i)
        rhoT     = -cv(1,i)/dv(2,i)
        hT       = dv(5,i)
        q2       = u*u + v*v
        theta    = CompTheta( dv(4,i),dv(3,i),q2 )
        a1       = cv(1,i)*rhop*hT + rhoT
        ra1g     = 1.D0/(cv(1,i)*theta*hT+rhoT)
        a4       = a1*ra1g
        a5       = cv(1,i)*hT*ra1g
        vc       = (sx*u+sy*v)/ds
        cs       = Sqrt((vc*vc)*((a4-1.D0)*(a4-1.D0))+4.D0*a5)
        fmue     = dv(6,i)/prlam
        f1       = (4.D0*rrho)/3.D0
        f2       = dv(4,i)*rrho
        fac      = Max(f1,f2)
        dtv      = (fac*fmue)/vol(i)
        lambdac  = 0.5D0*((a4+1.D0)*vc+cs)*ds
        lambdav  = dtv*(sx*sx+sy*sy)
        tstep(i) = vol(i)/(lambdac+cfac*lambdav)
      enddo

! - Navier-Stokes equations (laminar)

    else
      do i=1,nndint
        sx       = sproj(1,i)
        sy       = sproj(2,i)
        rrho     = 1.D0/cv(1,i)
        u        = Abs(cv(2,i)*rrho)
        v        = Abs(cv(3,i)*rrho)
        fmue     = dv(6,i)/prlam
        f1       = (4.D0*rrho)/3.D0
        f2       = dv(4,i)*rrho
        fac      = Max(f1,f2)
        dtv      = (fac*fmue)/vol(i)
        vc       = sx*u + sy*v
        cs       = dv(3,i)*(sx+sy)
        lambdac  = vc + cs
        lambdav  = dtv*(sx*sx+sy*sy)
        tstep(i) = vol(i)/(lambdac+cfac*lambdav)
      enddo
    endif  ! kprecond

  endif    ! kequs

! in case of global time-stepping - find min. time step in domain -------------

  if (ktimst == "G") then
    tsmin = 1.D+32
    do i=1,nndint
      tsmin = Min(tsmin,tstep(i))
    enddo
    do i=1,nndint
      tstep(i) = tsmin
    enddo
  endif

end subroutine TimeStep

!> @file solver.f90
!!
!! Single iteration of the governing equations.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: September 1, 2014
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

!> Integrates the four basic equations (continuity, momentum and energy) by
!! the explicit, multi-stage (Runge-Kutta) time-stepping scheme.
!!
!! @param work  work space for temporary variables
!!
subroutine Solver( work )

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  use ModInterfaces, only : BoundaryConditions, CompTheta, Cons2Prim, &
                            DependentVarsAll, DissipCentral, DissipCentralPrec,&
                            DissipRoe1, DissipRoe1Prec, DissipRoe2, &
                            DissipRoe2Prec, ErrorMessage, FluxCentral, &
                            FluxRoe1, FluxRoe2, FluxViscous, GradsFacesI, &
                            GradsFacesJ, GradsInitial, Irsmoo, &
                            MatrixTimesInverse, Prim2Cons, TimeStep, &
                            VarDifferences
  implicit none

! parameters
  real(rtype) :: work(:)

! local variables
  integer     :: i, j, irk, mp, mp2
  real(rtype) :: blend1, fac, adtv, H, q2, rhop, rhoT, hT, theta, u, v
  real(rtype) :: wvec(5), wpvec(5), pmat(5,5), gmat1(5,5), dmat(5,5), r(5)
  real(rtype), allocatable :: udum(:,:), vdum(:,:)

! *****************************************************************************
! calculate dimensions for dummy arrays (FaceGradients, Dissip*, Flux*,
! Irsmoo, BoundaryConditions); check them

  mp  = Max(imax,jmax) + 1
  mp2 = imax*jmax
  if ((mp+1)>Ubound(work,1) .or. (2*mp2)>Ubound(work,1)) then
    call ErrorMessage( "insufficient work space in Solver" )
  endif

! store previous solution; set dissipation = 0

  cvold(:,:,:) = cv(:,:,:)
  diss(:,:,:)  = 0.D0

! compute spectral radii and the time step

  call TimeStep

! loop over the Runge-Kutta stages ============================================

  do irk=1,nrk

! - initialize dissipation

    if (irk>1 .and. ldiss(irk)/=0) then
      blend1 = 1.D0 - betrk(irk)
      do j=2,j2
        do i=2,i2
          diss(1,i,j) = blend1*diss(1,i,j)
          diss(2,i,j) = blend1*diss(2,i,j)
          diss(3,i,j) = blend1*diss(3,i,j)
          diss(4,i,j) = blend1*diss(4,i,j)
        enddo
      enddo
    endif

! - viscous flux (Navier-Stokes eqs.)

    if (ldiss(irk)/=0 .and. kequs=="N") then
      udum = Reshape( work(1:mp2)        ,(/imax, jmax/) )
      vdum = Reshape( work((mp2+1):2*mp2),(/imax, jmax/) )
      call GradsInitial( udum,vdum )
      call GradsFacesI( udum,vdum )
      call GradsFacesJ( udum,vdum )
      call FluxViscous( betrk(irk),udum,vdum )
    endif

! - central scheme with artificial dissipation

    if (kdissip == "C") then

      ! artificial dissipation
      if (ldiss(irk) /= 0) then
        if (kprecond == "Y") then
          call DissipCentralPrec( betrk(irk),work )
        else
          call DissipCentral( betrk(irk),work )
        endif
      endif

      ! convective flux; add dissipation => residual
      call FluxCentral

! - Roe's flux-difference splitting scheme (upwind)

    else if (kdissip == "R") then

      ! differences of primitive variables
      if (iorder > 1) then
        call VarDifferences
      endif

      ! upwind dissipation
      if (ldiss(irk) /= 0) then
        if (iorder < 2) then
          if (kprecond == "Y") then
            call DissipRoe1Prec( betrk(irk) )
          else
            call DissipRoe1( betrk(irk) )
          endif
        else
          if (kprecond == "Y") then
            call DissipRoe2Prec( betrk(irk) )
          else
            call DissipRoe2( betrk(irk) )
          endif
        endif
      endif

      ! convective flux; add upwind dissipation => residual
      if (iorder < 2) then
        call FluxRoe1
      else
        call FluxRoe2
      endif

    endif ! kdissip

! - preconditioning

    if (kprecond == "Y") then
      do j=2,j2
        do i=2,i2
          rhop  =  cv(1,i,j)/dv(1,i,j)
          rhoT  = -cv(1,i,j)/dv(2,i,j)
          hT    = dv(5,i,j)
          u     = cv(2,i,j)/cv(1,i,j)
          v     = cv(3,i,j)/cv(1,i,j)
          q2    = u*u + v*v
          H     = (cv(4,i,j)+dv(1,i,j))/cv(1,i,j)
          theta = CompTheta( dv(4,i,j),dv(3,i,j),q2 )

          wvec(1)  = cv(1,i,j)
          wvec(2)  = cv(2,i,j)
          wvec(3)  = cv(3,i,j)
          wvec(4)  = 0.D0
          wvec(5)  = cv(4,i,j)
          wpvec(1) = dv(1,i,j)
          wpvec(2) = u
          wpvec(3) = v
          wpvec(4) = 0.D0
          wpvec(5) = dv(2,i,j)

          call Cons2Prim( wvec,wpvec,H,q2,theta,rhoT,0.D0,hT,gmat1 )
          call Prim2Cons( wvec,wpvec,H,rhop,rhoT,0.D0,hT,pmat )
          call MatrixTimesInverse( wpvec,q2,pmat,gmat1,dmat )
          r(1)       = rhs(1,i,j)
          r(2)       = rhs(2,i,j)
          r(3)       = rhs(3,i,j)
          r(4)       = rhs(4,i,j)
          rhs(1,i,j) = dmat(1,1)*r(1) + dmat(1,2)*r(2) + &
                       dmat(1,3)*r(3) + dmat(1,5)*r(4)
          rhs(2,i,j) = dmat(2,1)*r(1) + dmat(2,2)*r(2) + &
                       dmat(2,3)*r(3) + dmat(2,5)*r(4)
          rhs(3,i,j) = dmat(3,1)*r(1) + dmat(3,2)*r(2) + &
                       dmat(3,3)*r(3) + dmat(3,5)*r(4)
          rhs(4,i,j) = dmat(5,1)*r(1) + dmat(5,2)*r(2) + &
                       dmat(5,3)*r(3) + dmat(5,5)*r(4)
        enddo
      enddo
    endif

! - residual * time step / volume

    fac = ark(irk)*cfl
    do j=2,j2
      do i=2,i2
        adtv       = fac*tstep(i,j)/vol(i,j)
        rhs(1,i,j) = adtv*rhs(1,i,j)
        rhs(2,i,j) = adtv*rhs(2,i,j)
        rhs(3,i,j) = adtv*rhs(3,i,j)
        rhs(4,i,j) = adtv*rhs(4,i,j)
      enddo
    enddo

! - implicit residual smoothing

    if (epsirs > 0.D0) then
      call Irsmoo( work )
    endif

! - update - new solution, new dependent variables

    do j=2,j2
      do i=2,i2
        cv(1,i,j) = cvold(1,i,j) - rhs(1,i,j)
        cv(2,i,j) = cvold(2,i,j) - rhs(2,i,j)
        cv(3,i,j) = cvold(3,i,j) - rhs(3,i,j)
        cv(4,i,j) = cvold(4,i,j) - rhs(4,i,j)
      enddo
    enddo

    call DependentVarsAll

! - boundary conditions

    call BoundaryConditions( work )

  enddo ! irk

end subroutine Solver

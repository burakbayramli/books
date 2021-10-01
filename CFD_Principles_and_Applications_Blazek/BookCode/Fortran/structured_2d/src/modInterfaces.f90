!> @file modInterfaces.f90
!!
!! Explicit interfaces of all subroutines and functions.
!
! *****************************************************************************
!
!  (c) Jiri Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: September 2, 2014
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

!> Explicit function interfaces.
!!
module ModInterfaces

  implicit none
  interface

  subroutine AllocateMemory
  end subroutine AllocateMemory

  subroutine BcondCut( lb,lbeg,lend,lbs,lbegs,lends,nvars,var )
    use ModDataTypes
    integer, intent(in) :: lb, lbeg, lend, lbs, lbegs, lends, nvars
    real(rtype) :: var(:,0:,0:)
  end subroutine BcondCut

  subroutine BcondCutSingle( lb,lbeg,lend,lbs,lbegs,lends,var )
    use ModDataTypes
    integer, intent(in) :: lb, lbeg, lend, lbs, lbegs, lends
    real(rtype) :: var(0:,0:)
  end subroutine BcondCutSingle

  subroutine BcondFarfield( lb,lbeg,lend,rhof,uf,vf,pf )
    use ModDataTypes
    integer, intent(in) :: lb, lbeg, lend
    real(rtype) :: rhof(0:), uf(0:), vf(0:), pf(0:)
  end subroutine BcondFarfield

  subroutine BcondInflow( lb,lbeg,lend )
    integer, intent(in) :: lb, lbeg, lend
  end subroutine BcondInflow

  subroutine BcondInject( lb,lbeg,lend )
    integer, intent(in) :: lb, lbeg, lend
  end subroutine BcondInject

  subroutine BcondOutflow( lb,lbeg,lend )
    integer, intent(in) :: lb, lbeg, lend
  end subroutine BcondOutflow

  subroutine BcondSymmetry( lb,lbeg,lend )
    integer, intent(in) :: lb, lbeg, lend
  end subroutine BcondSymmetry

  subroutine BcondWalleu( lb,lbeg,lend )
    integer, intent(in) :: lb, lbeg, lend
  end subroutine BcondWalleu

  subroutine BcondWallns( lb,lbeg,lend )
    integer, intent(in) :: lb, lbeg, lend
  end subroutine BcondWallns

  subroutine BoundaryConditions( work )
    use ModDataTypes
    real(rtype) :: work(:)
  end subroutine BoundaryConditions

  function CompTheta( gam,c,q2 )
    use ModDataTypes
    real(rtype), intent(in) :: gam, c, q2
    real(rtype) :: CompTheta
  end function CompTheta

  subroutine Cons2Prim( wvec,wpvec,H,q2,theta,rhoT,hp,hT,pmat1 )
    use ModDataTypes
    real(rtype), intent( in) :: H, q2, theta, rhoT, hp, hT
    real(rtype), intent( in) :: wvec(5), wpvec(5)
    real(rtype), intent(out) :: pmat1(5,5)
  end subroutine Cons2Prim

  subroutine Convergence
  end subroutine Convergence

  subroutine DependentVarsAll
  end subroutine DependentVarsAll

  subroutine DependentVarsOne( i,j )
    integer, intent(in) :: i, j
  end subroutine DependentVarsOne

  subroutine DissipCentral( beta,dp )
    use ModDataTypes
    real(rtype), intent(in) :: beta
    real(rtype) :: dp(0:)
  end subroutine DissipCentral

  subroutine DissipCentralPrec( beta,dp )
    use ModDataTypes
    real(rtype), intent(in) :: beta
    real(rtype) :: dp(0:)
  end subroutine DissipCentralPrec

  subroutine DissipRoe1( beta )
    use ModDataTypes
    real(rtype), intent(in) :: beta
  end subroutine DissipRoe1

  subroutine DissipRoe1Prec( beta )
    use ModDataTypes
    real(rtype), intent(in) :: beta
  end subroutine DissipRoe1Prec

  subroutine DissipRoe2( beta )
    use ModDataTypes
    real(rtype), intent(in) :: beta
  end subroutine DissipRoe2

  subroutine DissipRoe2Prec( beta )
    use ModDataTypes
    real(rtype), intent(in) :: beta
  end subroutine DissipRoe2Prec

  subroutine ErrorMessage( message )
    character(*), intent(in) :: message
  end subroutine ErrorMessage

  subroutine FluxBoundary( lb,lbeg,lend,itype )
    integer, intent(in) :: lb, lbeg, lend, itype
  end subroutine FluxBoundary

  subroutine FluxCentral
  end subroutine FluxCentral

  subroutine FluxCentralBound( lb,lbeg,lend,itype )
    integer, intent(in) :: lb, lbeg, lend, itype
  end subroutine FluxCentralBound

  subroutine FluxRoe1
  end subroutine FluxRoe1

  subroutine FluxRoe2
  end subroutine FluxRoe2

  subroutine FluxRoeBound( lb,lbeg,lend,itype )
    integer, intent(in) :: lb, lbeg, lend, itype
  end subroutine FluxRoeBound

  subroutine FluxViscous( beta,u,v )
    use ModDataTypes
    real(rtype), intent(in) :: beta
    real(rtype) :: u(:,:), v(:,:)
  end subroutine FluxViscous

  subroutine Forces
  end subroutine Forces

  subroutine GradsFacesI( u,v )
    use ModDataTypes
    real(rtype) :: u(:,:), v(:,:)
  end subroutine GradsFacesI

  subroutine GradsFacesJ( u,v )
    use ModDataTypes
    real(rtype) :: u(:,:), v(:,:)
  end subroutine GradsFacesJ

  subroutine GradsInitial( u,v )
    use ModDataTypes
    real(rtype) :: u(:,:), v(:,:)
  end subroutine GradsInitial

  subroutine InitConstants
  end subroutine InitConstants

  subroutine InitMetrics
  end subroutine InitMetrics

  subroutine InitSolution
  end subroutine InitSolution

  subroutine Irsmoo( work )
    use ModDataTypes
    real(rtype) :: work(:)
  end subroutine Irsmoo


  subroutine LeftEigenvec( wvec,wpvec,nvec,V,theta,rhop,rhoT,hp,hT,evl )
    use ModDataTypes
    real(rtype), intent( in) :: V, theta, rhop, rhoT, hp, hT
    real(rtype), intent( in) :: wvec(5), wpvec(5), nvec(3)
    real(rtype), intent(out) :: evl(5,5)
  end subroutine LeftEigenvec

  subroutine LimiterRefvals
  end subroutine LimiterRefvals

  subroutine Massflow
  end subroutine Massflow

  subroutine MatprodTp1_P1( wvec,wpvec,nvec,V,H,theta,rhop,rhoT,hp,hT,q2,mat )
    use ModDataTypes
    real(rtype), intent( in) :: V, H, theta, rhop, rhoT, hp, hT, q2
    real(rtype), intent( in) :: wvec(5), wpvec(5), nvec(3)
    real(rtype), intent(out) :: mat(5,5)
  end subroutine MatprodTp1_P1

  subroutine MatrixTimesInverse( wpvec,q2,amat,bmat,cmat )
    use ModDataTypes
    real(rtype), intent( in) :: q2
    real(rtype), intent( in) :: wpvec(5), amat(5,5), bmat(5,5)
    real(rtype), intent(out) :: cmat(5,5)
  end subroutine MatrixTimesInverse

  subroutine MatVecProd5( a,v,c )
    use ModDataTypes
    real(rtype), intent( in) :: a(5,5), v(5)
    real(rtype), intent(out) :: c(5)
  end subroutine MatVecProd5

  subroutine PlotFlow
  end subroutine PlotFlow

  subroutine PlotSurfaces
  end subroutine PlotSurfaces

  subroutine Prim2Cons( wvec,wpvec,H,theta,rhoT,hp,hT,pmat )
    use ModDataTypes
    real(rtype), intent( in) :: H, theta, rhoT, hp, hT
    real(rtype), intent( in) :: wvec(5), wpvec(5)
    real(rtype), intent(out) :: pmat(5,5)
  end subroutine Prim2Cons

  subroutine PrintParams
  end subroutine PrintParams

  function ReadChar( iunit )
    integer, intent(in) :: iunit
    character(1) :: ReadChar
  end function ReadChar

  subroutine ReadGrid
  end subroutine ReadGrid

  subroutine ReadParams( fname )
    character(*), intent(in) :: fname
  end subroutine ReadParams

  subroutine ReadSolution
  end subroutine ReadSolution

  subroutine ReadTopology
  end subroutine ReadTopology

  subroutine RightEigenvec( wvec,wpvec,nvec,V,H,theta,rhop,rhoT,hp,hT,evr )
    use ModDataTypes
    real(rtype), intent( in) :: V, H, theta, rhop, rhoT, hp, hT
    real(rtype), intent( in) :: wvec(5), wpvec(5), nvec(3)
    real(rtype), intent(out) :: evr(5,5)
  end subroutine RightEigenvec

  subroutine Solver( work )
    use ModDataTypes
    real(rtype) :: work(:)
  end subroutine Solver

  subroutine TimeStep
  end subroutine TimeStep

  subroutine VarDifferences
  end subroutine VarDifferences

  subroutine WriteSolution
  end subroutine WriteSolution

  end interface

end module ModInterfaces

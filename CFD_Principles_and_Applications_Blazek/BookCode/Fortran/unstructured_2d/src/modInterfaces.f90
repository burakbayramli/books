!> @file modInterfaces.f90
!!
!! Explicit interfaces of all subroutines and functions.
!
! *****************************************************************************
!
!  (c) Jiri Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: September 19, 2014
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

  subroutine BcondFarfield( ibegn,iendn,rhof,uf,vf,pf )
    use ModDataTypes
    integer, intent(in) :: ibegn, iendn
    real(rtype) :: rhof(:), uf(:), vf(:), pf(:)
  end subroutine BcondFarfield

  subroutine BcondInflow( ibegn,iendn )
    integer, intent(in) :: ibegn, iendn
  end subroutine BcondInflow

  subroutine BcondOutflow( ibegn,iendn )
    integer, intent(in) :: ibegn, iendn
  end subroutine BcondOutflow

  subroutine BcondWallns( ibegn,iendn )
    integer, intent(in) :: ibegn, iendn
  end subroutine BcondWallns

  subroutine BoundaryConditions( work )
    use ModDataTypes
    real(rtype) :: work(:)
  end subroutine BoundaryConditions

  subroutine CheckMetrics( work )
    use ModDataTypes
    real(rtype) :: work(:)
  end subroutine CheckMetrics

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

  subroutine DependentVarsOne( i )
    integer, intent(in) :: i
  end subroutine DependentVarsOne

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

  subroutine DummyNodes
  end subroutine DummyNodes

  subroutine EdgesFinalize( niedge,iedge )
    integer :: niedge(:), iedge(:,:)
  end subroutine EdgesFinalize

  subroutine EdgesInitialize( niedge,iedge )
    integer, intent(out) :: niedge(:), iedge(:,:)
  end subroutine EdgesInitialize

  subroutine ErrorMessage( message )
    character(*), intent(in) :: message
  end subroutine ErrorMessage

  subroutine FaceVectorsSymm( marker )
    integer :: marker(:)
  end subroutine FaceVectorsSymm

  subroutine FluxViscous( beta )
    use ModDataTypes
    real(rtype), intent(in) :: beta
  end subroutine FluxViscous

  subroutine FluxWalls
  end subroutine FluxWalls

  subroutine Forces
  end subroutine Forces

  subroutine FluxRoe1
  end subroutine FluxRoe1

  subroutine FluxRoe2
  end subroutine FluxRoe2

  subroutine Gradients
  end subroutine Gradients

  subroutine GradientsVisc
  end subroutine GradientsVisc

  subroutine InitConstants
  end subroutine InitConstants

  subroutine InitMetrics( niedge,iedge )
    integer, intent(in) :: niedge(:), iedge(:,:)
  end subroutine InitMetrics

  subroutine InitMetricsBound( marker,btria )
    integer :: marker(:), btria(:,:)
  end subroutine InitMetricsBound

  subroutine InitSolution
  end subroutine InitSolution

  subroutine Irsmoo( ncontr,rhsold,rhsit )
    use ModDataTypes
    integer     :: ncontr(:)
    real(rtype) :: rhsold(:,:), rhsit(:,:)
  end subroutine Irsmoo

  subroutine LeftEigenvec( wvec,wpvec,nvec,V,theta,rhop,rhoT,hp,hT,evl )
    use ModDataTypes
    real(rtype), intent( in) :: V, theta, rhop, rhoT, hp, hT
    real(rtype), intent( in) :: wvec(5), wpvec(5), nvec(3)
    real(rtype), intent(out) :: evl(5,5)
  end subroutine LeftEigenvec

  subroutine Limiter( umin,umax )
    use ModDataTypes
    real(rtype), intent(in) :: umin(:,:), umax(:,:)
  end subroutine Limiter

  subroutine LimiterInit( umin,umax )
    use ModDataTypes
    real(rtype), intent(out) :: umin(:,:), umax(:,:)
  end subroutine LimiterInit

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

  subroutine Periodic( var )
    use ModDataTypes
    real(rtype) :: var(:,:)
  end subroutine Periodic

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

  subroutine RightEigenvec( wvec,wpvec,nvec,V,H,theta,rhop,rhoT,hp,hT,evr )
    use ModDataTypes
    real(rtype), intent( in) :: V, H, theta, rhop, rhoT, hp, hT
    real(rtype), intent( in) :: wvec(5), wpvec(5), nvec(3)
    real(rtype), intent(out) :: evr(5,5)
  end subroutine RightEigenvec

  subroutine Solver( iwork,work )
    use ModDataTypes
    integer     :: iwork(:)
    real(rtype) :: work(:)
  end subroutine Solver

  subroutine TimeStep
  end subroutine TimeStep

  subroutine VolumeProjections
  end subroutine VolumeProjections

  subroutine WriteSolution
  end subroutine WriteSolution

  subroutine ZeroResiduals
  end subroutine ZeroResiduals

  end interface

end module ModInterfaces

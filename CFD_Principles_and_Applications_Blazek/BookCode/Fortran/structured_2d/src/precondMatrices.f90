!> @file precondMatrices.f90
!!
!! Collection of matrices and operators for low Mach-number preconditioning.
!! All formulations assume general 3-D flow.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: March 7, 2014
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

!> Computes the preconditioning parameter theta.
!!
!! @param gam  ratio of specific heats
!! @param c    speed of sound
!! @param q2   total velocity squared
!!
function CompTheta( gam,c,q2 )

  use ModDataTypes
  use ModNumerics
  use ModPhysics
  implicit none

! parameters
  real(rtype), intent(in) :: gam, c, q2

! result
  real(rtype) :: CompTheta

! local variables
  real(rtype) :: mach2, mref2, beta

! *****************************************************************************

  mach2 = q2/(c*c)
  mref2 = Max(Min(mach2,1.D0),precoeff*machinf*machinf)
  beta  = mref2/(1.D0+(gam-1.D0)*mref2)

  CompTheta = 1.D0/(beta*c*c)

end function CompTheta

! =============================================================================

!> Computes transformation matrix from primitive to conservative
!! variables P (equivalent to the preconditioning matrix G).
!!
!! @param wvec   vector of conservative variables
!! @param wpvec  vector of primitive variables
!! @param H      total enthalpy
!! @param theta  preconditioning parameter
!! @param rhoT   derivative of density wrp. to temperature
!! @param hp     derivative of enthalpy wrp. to pressure
!! @param hT     derivative of enthalpy wrp. to temperature
!! @param pmat   matrix P
!!
subroutine Prim2Cons( wvec,wpvec,H,theta,rhoT,hp,hT,pmat )

  use ModDataTypes
  implicit none

! parameters
  real(rtype), intent( in) :: H, theta, rhoT, hp, hT
  real(rtype), intent( in) :: wvec(5), wpvec(5)
  real(rtype), intent(out) :: pmat(5,5)

! *****************************************************************************

  pmat(1,1) = theta
  pmat(2,1) = theta*wpvec(2)
  pmat(3,1) = theta*wpvec(3)
  pmat(4,1) = theta*wpvec(4)
  pmat(5,1) = theta*H - 1.D0 - wvec(1)*hp

  pmat(1,2) = 0.D0
  pmat(2,2) = wvec(1)
  pmat(3,2) = 0.D0
  pmat(4,2) = 0.D0
  pmat(5,2) = wvec(2)

  pmat(1,3) = 0.D0
  pmat(2,3) = 0.D0
  pmat(3,3) = wvec(1)
  pmat(4,3) = 0.D0
  pmat(5,3) = wvec(3)

  pmat(1,4) = 0.D0
  pmat(2,4) = 0.D0
  pmat(3,4) = 0.D0
  pmat(4,4) = wvec(1)
  pmat(5,4) = wvec(4)

  pmat(1,5) = rhoT
  pmat(2,5) = rhoT*wpvec(2)
  pmat(3,5) = rhoT*wpvec(3)
  pmat(4,5) = rhoT*wpvec(4)
  pmat(5,5) = rhoT*H + wvec(1)*hT

end subroutine Prim2Cons

! =============================================================================

!> Computes transformation matrix from conservative to primitive variables
!! P^-1 (equivalent to the inverse of the preconditioning matrix G^-1).
!!
!! @param wvec   vector of conservative variables
!! @param wpvec  vector of primitive variables
!! @param H      total enthalpy
!! @param q2     total velocity squared
!! @param theta  preconditioning parameter
!! @param rhoT   derivative of density wrp. to temperature
!! @param hp     derivative of enthalpy wrp. to pressure
!! @param hT     derivative of enthalpy wrp. to temperature
!! @param pmat1  inverse of matrix P
!!
subroutine Cons2Prim( wvec,wpvec,H,q2,theta,rhoT,hp,hT,pmat1 )

  use ModDataTypes
  implicit none

! parameters
  real(rtype), intent( in) :: H, q2, theta, rhoT, hp, hT
  real(rtype), intent( in) :: wvec(5), wpvec(5)
  real(rtype), intent(out) :: pmat1(5,5)

! local variables
  real(rtype) :: rrho, Hq2, ra1, d1, d2

! *****************************************************************************

  rrho = 1.D0/wvec(1)
  Hq2  = H - q2
  ra1  = 1.D0/(wvec(1)*theta*hT + rhoT*(1.D0-wvec(1)*hp))
  d1   = rhoT*ra1
  d2   = theta*ra1

  pmat1(1,1) = ra1*(wvec(1)*hT+rhoT*Hq2)
  pmat1(2,1) = -wpvec(2)*rrho
  pmat1(3,1) = -wpvec(3)*rrho
  pmat1(4,1) = -wpvec(4)*rrho
  pmat1(5,1) = ra1*(1.D0-theta*Hq2-wvec(1)*hp)

  pmat1(1,2) = d1*wpvec(2)
  pmat1(2,2) = rrho
  pmat1(3,2) = 0.D0
  pmat1(4,2) = 0.D0
  pmat1(5,2) = -d2*wpvec(2)

  pmat1(1,3) = d1*wpvec(3)
  pmat1(2,3) = 0.D0
  pmat1(3,3) = rrho
  pmat1(4,3) = 0.D0
  pmat1(5,3) = -d2*wpvec(3)

  pmat1(1,4) = d1*wpvec(4)
  pmat1(2,4) = 0.D0
  pmat1(3,4) = 0.D0
  pmat1(4,4) = rrho
  pmat1(5,4) = -d2*wpvec(4)

  pmat1(1,5) = -d1
  pmat1(2,5) = 0.D0
  pmat1(3,5) = 0.D0
  pmat1(4,5) = 0.D0
  pmat1(5,5) = d2

end subroutine Cons2Prim

! =============================================================================

!> Computes matrix of left eigenvectors of G^-1*A_c,p (i.e., (T_p)^-1).
!!
!! @param wvec   vector of conservative variables
!! @param wpvec  vector of primitive variables
!! @param nvec   components of the unit normal vector
!! @param V      contravariant velocity
!! @param theta  preconditioning parameter
!! @param rhop   derivative of density wrp. to pressure
!! @param rhoT   derivative of density wrp. to temperature
!! @param hp     derivative of enthalpy wrp. to pressure
!! @param hT     derivative of enthalpy wrp. to temperature
!! @param evl    matrix of left eigenvectors (T_p)^-1
!!
subroutine LeftEigenvec( wvec,wpvec,nvec,V,theta,rhop,rhoT,hp,hT,evl )

  use ModDataTypes
  implicit none

! parameters
  real(rtype), intent( in) :: V, theta, rhop, rhoT, hp, hT
  real(rtype), intent( in) :: wvec(5), wpvec(5), nvec(3)
  real(rtype), intent(out) :: evl(5,5)

! local variables
  real(rtype) :: a1, a1g, ra1g, a4, a5, a6, a7, cc, h1, h2, h3

! *****************************************************************************

  a1   = wvec(1)*rhop*hT + rhoT*(1.D0-wvec(1)*hp)
  a1g  = wvec(1)*theta*hT + rhoT*(1.D0-wvec(1)*hp)
  ra1g = 1.D0/a1g
  a4   = a1*ra1g
  a5   = wvec(1)*hT*ra1g
  cc   = 0.5D0*Sqrt((V*V)*(a4-1.D0)*(a4-1.D0)+4.D0*a5)
  a6   = wvec(1)*a5
  a7   = (V*(a4-1.D0))/(4.D0*cc)
  h1   = rhoT*ra1g
  h2   = a6/cc
  h3   = a6/wpvec(5)

  evl(1,1) = -h1*nvec(1)
  evl(2,1) = -h1*nvec(2)
  evl(3,1) = -h1*nvec(3)
  evl(4,1) =  0.5D0 + a7
  evl(5,1) =  0.5D0 - a7

  evl(1,2) =  0.D0
  evl(2,2) = -h2*nvec(3)
  evl(3,2) =  h2*nvec(2)
  evl(4,2) =  0.5D0*h2*nvec(1)
  evl(5,2) = -0.5D0*h2*nvec(1)

  evl(1,3) =  h2*nvec(3)
  evl(2,3) =  0.D0
  evl(3,3) = -h2*nvec(1)
  evl(4,3) =  0.5D0*h2*nvec(2)
  evl(5,3) = -0.5D0*h2*nvec(2)

  evl(1,4) = -h2*nvec(2)
  evl(2,4) =  h2*nvec(1)
  evl(3,4) =  0.D0
  evl(4,4) =  0.5D0*h2*nvec(3)
  evl(5,4) = -0.5D0*h2*nvec(3)

  evl(1,5) = -h3*nvec(1)
  evl(2,5) = -h3*nvec(2)
  evl(3,5) = -h3*nvec(3)
  evl(4,5) =  0.D0
  evl(5,5) =  0.D0

end subroutine LeftEigenvec

! =============================================================================

!> Computes matrix of right eigenvectors of G^-1*A_c,p multiplied
!! by the preconditioning matrix G (i.e., G*T_p).
!!
!! @param wvec   vector of conservative variables
!! @param wpvec  vector of primitive variables
!! @param nvec   components of the unit normal vector
!! @param V      contravariant velocity
!! @param H      total enthalpy
!! @param theta  preconditioning parameter
!! @param rhop   derivative of density wrp. to pressure
!! @param rhoT   derivative of density wrp. to temperature
!! @param hp     derivative of enthalpy wrp. to pressure
!! @param hT     derivative of enthalpy wrp. to temperature
!! @param evr    matrix of right eigenvectors multiplied by G (G*T_p)
!!
subroutine RightEigenvec( wvec,wpvec,nvec,V,H,theta,rhop,rhoT,hp,hT,evr )

  use ModDataTypes
  implicit none

! parameters
  real(rtype), intent( in) :: V, H, theta, rhop, rhoT, hp, hT
  real(rtype), intent( in) :: wvec(5), wpvec(5), nvec(3)
  real(rtype), intent(out) :: evr(5,5)

! local variables
  real(rtype) :: a1, a1g, ra1g, a4, a5, a8, a9, a10, cc, h1, h2, h3

! *****************************************************************************

  a1   = wvec(1)*rhop*hT + rhoT*(1.D0-wvec(1)*hp)
  a1g  = wvec(1)*theta*hT + rhoT*(1.D0-wvec(1)*hp)
  ra1g = 1.D0/a1g
  a4   = a1*ra1g
  a5   = wvec(1)*hT*ra1g
  cc   = 0.5D0*Sqrt((V*V)*(a4-1.D0)*(a4-1.D0)+4.D0*a5)
  a8   = rhoT*wpvec(5)/wvec(1)
  a9   = -0.5D0*V*(a4-1.D0)
  a10  = a8*H + hT*wpvec(5)
  h1   = a9 + cc
  h2   = a9 - cc
  h3   = a1g/(wvec(1)*hT)

  evr(1,1) = -a8*nvec(1)
  evr(2,1) = -a8*wpvec(2)*nvec(1)
  evr(3,1) =  cc*nvec(3) - a8*wpvec(3)*nvec(1)
  evr(4,1) = -cc*nvec(2) - a8*wpvec(4)*nvec(1)
  evr(5,1) =  cc*(wpvec(3)*nvec(3)-wpvec(4)*nvec(2)) - a10*nvec(1)

  evr(1,2) = -a8*nvec(2)
  evr(2,2) = -cc*nvec(3) - a8*wpvec(2)*nvec(2)
  evr(3,2) = -a8*wpvec(3)*nvec(2)
  evr(4,2) =  cc*nvec(1) - a8*wpvec(4)*nvec(2)
  evr(5,2) =  cc*(wpvec(4)*nvec(1)-wpvec(2)*nvec(3)) - a10*nvec(2)

  evr(1,3) = -a8*nvec(3)
  evr(2,3) =  cc*nvec(2) - a8*wpvec(2)*nvec(3)
  evr(3,3) = -cc*nvec(1) - a8*wpvec(3)*nvec(3)
  evr(4,3) = -a8*wpvec(4)*nvec(3)
  evr(5,3) =  cc*(wpvec(2)*nvec(2)-wpvec(3)*nvec(1)) - a10*nvec(3)

  evr(1,4) = h3
  evr(2,4) = wpvec(2) + h1*nvec(1)
  evr(3,4) = wpvec(3) + h1*nvec(2)
  evr(4,4) = wpvec(4) + h1*nvec(3)
  evr(5,4) = H + h1*V

  evr(1,5) = h3
  evr(2,5) = wpvec(2) + h2*nvec(1)
  evr(3,5) = wpvec(3) + h2*nvec(2)
  evr(4,5) = wpvec(4) + h2*nvec(3)
  evr(5,5) = H + h2*V

  evr(1,1) = evr(1,1)*h3
  evr(1,2) = evr(1,2)*h3
  evr(1,3) = evr(1,3)*h3

  evr(2,1) = evr(2,1)*h3
  evr(2,2) = evr(2,2)*h3
  evr(2,3) = evr(2,3)*h3
  evr(2,4) = evr(2,4)*h3
  evr(2,5) = evr(2,5)*h3

  evr(3,1) = evr(3,1)*h3
  evr(3,2) = evr(3,2)*h3
  evr(3,3) = evr(3,3)*h3
  evr(3,4) = evr(3,4)*h3
  evr(3,5) = evr(3,5)*h3

  evr(4,1) = evr(4,1)*h3
  evr(4,2) = evr(4,2)*h3
  evr(4,3) = evr(4,3)*h3
  evr(4,4) = evr(4,4)*h3
  evr(4,5) = evr(4,5)*h3

  evr(5,1) = evr(5,1)*h3
  evr(5,2) = evr(5,2)*h3
  evr(5,3) = evr(5,3)*h3
  evr(5,4) = evr(5,4)*h3
  evr(5,5) = evr(5,5)*h3

end subroutine RightEigenvec

! =============================================================================

!> Computes matrix product (T_p^-1) * (P^-1).
!!
!! @param wvec   vector of conservative variables
!! @param wpvec  vector of primitive variables
!! @param nvec   components of the unit normal vector
!! @param V      contravariant velocity
!! @param H      total enthalpy
!! @param theta  preconditioning parameter
!! @param rhop   derivative of density wrp. to pressure
!! @param rhoT   derivative of density wrp. to temperature
!! @param hp     derivative of enthalpy wrp. to pressure
!! @param hT     derivative of enthalpy wrp. to temperature
!! @param q2     total velocity squared
!! @param mat    resulting matrix
!!
subroutine MatprodTp1_P1( wvec,wpvec,nvec,V,H,theta,rhop,rhoT,hp,hT,q2,mat )

  use ModDataTypes
  implicit none

! parameters
  real(rtype), intent( in) :: V, H, theta, rhop, rhoT, hp, hT, q2
  real(rtype), intent( in) :: wvec(5), wpvec(5), nvec(3)
  real(rtype), intent(out) :: mat(5,5)

! local variables
  real(rtype) :: a1, a1g, ra1g, a4, a5, a5rp, a5c, a5c5, a5rt2, a7, cc, &
                 a14, a15, a15rt, a16, a16rt, a17, a17rt, h0, rhoT2, vc

! *****************************************************************************

  a1    = wvec(1)*rhop*hT + rhoT*(1.D0-wvec(1)*hp)
  a1g   = wvec(1)*theta*hT + rhoT*(1.D0-wvec(1)*hp)
  ra1g  = 1.D0/a1g
  a4    = a1*ra1g
  a5    = wvec(1)*hT*ra1g
  cc    = 0.5D0*Sqrt((V*V)*(a4-1.D0)*(a4-1.D0)+4.D0*a5)
  a7    = V*(a4-1.D0)/(4.D0*cc)
  h0    = (a5*wvec(1))/(a1*wpvec(5))
  a5rp  = rhop*h0
  a5c   = a5/cc
  a5c5  = 0.5D0*a5c
  rhoT2 = (rhoT*rhoT)/(a1*a1g)
  a5rt2 = a5rp - rhoT2
  a14   = h0*(1.D0-rhop*(H-q2)-wvec(1)*hp)
  a15   = (H-q2)*rhoT + wvec(1)*hT
  a15rt = rhoT*a15/(a1*a1g)
  a16   = (0.5D0+a7)/a1
  a16rt = a16*rhoT
  a17   = (0.5D0-a7)/a1
  a17rt = a17*rhoT
  vc    = nvec(1)*wpvec(2) + nvec(2)*wpvec(3) + nvec(3)*wpvec(4)

  mat(1,1) = a5c*(nvec(2)*wpvec(4)-nvec(3)*wpvec(3)) - nvec(1)*(a14+a15rt)
  mat(1,2) = (wpvec(2)*nvec(1))*a5rt2
  mat(1,3) = (wpvec(3)*nvec(1))*a5rt2 + a5c*nvec(3)
  mat(1,4) = (wpvec(4)*nvec(1))*a5rt2 - a5c*nvec(2)
  mat(1,5) = -nvec(1)*a5rt2

  mat(2,1) = a5c*(nvec(3)*wpvec(2)-nvec(1)*wpvec(4)) - nvec(2)*(a14+a15rt)
  mat(2,2) = (wpvec(2)*nvec(2))*a5rt2 - a5c*nvec(3)
  mat(2,3) = (wpvec(3)*nvec(2))*a5rt2
  mat(2,4) = (wpvec(4)*nvec(2))*a5rt2 + a5c*nvec(1)
  mat(2,5) = -nvec(2)*a5rt2

  mat(3,1) = a5c*(nvec(1)*wpvec(3)-nvec(2)*wpvec(2)) - nvec(3)*(a14+a15rt)
  mat(3,2) = (wpvec(2)*nvec(3))*a5rt2 + a5c*nvec(2)
  mat(3,3) = (wpvec(3)*nvec(3))*a5rt2 - a5c*nvec(1)
  mat(3,4) = (wpvec(4)*nvec(3))*a5rt2
  mat(3,5) = -nvec(3)*a5rt2

  mat(4,1) = a15*a16 - a5c5*vc
  mat(4,2) = a16rt*wpvec(2) + a5c5*nvec(1)
  mat(4,3) = a16rt*wpvec(3) + a5c5*nvec(2)
  mat(4,4) = a16rt*wpvec(4) + a5c5*nvec(3)
  mat(4,5) = -a16rt

  mat(5,1) = a15*a17 + a5c5*vc
  mat(5,2) = a17rt*wpvec(2) - a5c5*nvec(1)
  mat(5,3) = a17rt*wpvec(3) - a5c5*nvec(2)
  mat(5,4) = a17rt*wpvec(4) - a5c5*nvec(3)
  mat(5,5) = -a17rt

end subroutine MatprodTp1_P1

! =============================================================================

!> Computes matrix times the inverse of a similar matrix, where both matrices
!! have the structure of the preconditioning (or the transformation) matrix.
!! Thus, products like P*G^-1 or G*P^-1 can be evaluated efficiently.
!!
!! @param wpvec  vector of primitive variables
!! @param q2     total velocity squared
!! @param amat   matrix A
!! @param bmat   matrix B
!! @param cmat   resulting matrix C, i.e., C=A*B
!!
subroutine MatrixTimesInverse( wpvec,q2,amat,bmat,cmat )

  use ModDataTypes
  implicit none

! parameters
  real(rtype), intent( in) :: q2
  real(rtype), intent( in) :: wpvec(5), amat(5,5), bmat(5,5)
  real(rtype), intent(out) :: cmat(5,5)

! local variables
  real(rtype) :: a1, a2, a3, a4, a5

! *****************************************************************************

  a1 = amat(1,1)*bmat(1,1) + amat(1,5)*bmat(5,1)
  a2 = amat(1,1)*bmat(1,2) + amat(1,5)*bmat(5,2)
  a3 = amat(1,1)*bmat(1,3) + amat(1,5)*bmat(5,3)
  a4 = amat(1,1)*bmat(1,4) + amat(1,5)*bmat(5,4)
  a5 = amat(1,1)*bmat(1,5) + amat(1,5)*bmat(5,5)

  cmat(1,1) = a1
  cmat(2,1) = wpvec(2)*a1 - wpvec(2)
  cmat(3,1) = wpvec(3)*a1 - wpvec(3)
  cmat(4,1) = wpvec(4)*a1 - wpvec(4)
  cmat(5,1) = amat(5,1)*bmat(1,1) + amat(5,5)*bmat(5,1) - q2

  cmat(1,2) = a2
  cmat(2,2) = wpvec(2)*a2 + 1.D0
  cmat(3,2) = wpvec(3)*a2
  cmat(4,2) = wpvec(4)*a2
  cmat(5,2) = amat(5,1)*bmat(1,2) + amat(5,5)*bmat(5,2) + wpvec(2)

  cmat(1,3) = a3
  cmat(2,3) = wpvec(2)*a3
  cmat(3,3) = wpvec(3)*a3 + 1.D0
  cmat(4,3) = wpvec(4)*a3
  cmat(5,3) = amat(5,1)*bmat(1,3) + amat(5,5)*bmat(5,3) + wpvec(3)

  cmat(1,4) = a4
  cmat(2,4) = wpvec(2)*a4
  cmat(3,4) = wpvec(3)*a4
  cmat(4,4) = wpvec(4)*a4 + 1.D0
  cmat(5,4) = amat(5,1)*bmat(1,4) + amat(5,5)*bmat(5,4) + wpvec(4)

  cmat(1,5) = a5
  cmat(2,5) = wpvec(2)*a5
  cmat(3,5) = wpvec(3)*a5
  cmat(4,5) = wpvec(4)*a5
  cmat(5,5) = amat(5,1)*bmat(1,5) + amat(5,5)*bmat(5,5)

end subroutine MatrixTimesInverse

! =============================================================================

!> Computes matrix times vector (n=5).
!!
!! @param a  5x5 matrix A
!! @param v  vector v (length 5)
!! @param c  resulting vector c, i.e., c=A*v
!!
subroutine MatVecProd5( a,v,c )

  use ModDataTypes
  implicit none

! parameters
  real(rtype), intent( in) :: a(5,5), v(5)
  real(rtype), intent(out) :: c(5)

! *****************************************************************************

  c(1) = a(1,1)*v(1) + a(1,2)*v(2) + a(1,3)*v(3) + a(1,4)*v(4) + a(1,5)*v(5)
  c(2) = a(2,1)*v(1) + a(2,2)*v(2) + a(2,3)*v(3) + a(2,4)*v(4) + a(2,5)*v(5)
  c(3) = a(3,1)*v(1) + a(3,2)*v(2) + a(3,3)*v(3) + a(3,4)*v(4) + a(3,5)*v(5)
  c(4) = a(4,1)*v(1) + a(4,2)*v(2) + a(4,3)*v(3) + a(4,4)*v(4) + a(4,5)*v(5)
  c(5) = a(5,1)*v(1) + a(5,2)*v(2) + a(5,3)*v(3) + a(5,4)*v(4) + a(5,5)*v(5)

end subroutine MatVecProd5

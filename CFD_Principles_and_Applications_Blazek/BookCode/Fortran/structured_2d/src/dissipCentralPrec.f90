!> @file dissipCentralPrec.f90
!!
!! Computation of central artificial dissipation with preconditioning.
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

!> Computes artificial dissipation as a blend of 2nd- and 4th-order central
!! differences. This corresponds to the JST scheme. The dissipation terms are
!! multiplied by a preconditioning matrix for low Mach numbers (see Eq. (9.52)).
!!
!! @param beta  coefficient for mixing new and old dissipation values
!! @param dp    temporary storage for the pressure sensor
!!
subroutine DissipCentralPrec( beta,dp )

  use ModDataTypes
  use ModGeometry
  use ModNumerics
  use ModPhysics
  use ModInterfaces, only : CompTheta, Cons2Prim, MatrixTimesInverse, Prim2Cons
  implicit none

! parameters
  real(rtype), intent(in) :: beta
  real(rtype) :: dp(0:)

! local variables
  integer     :: i, j, im1, ip1, ip2, jm1, jp1, jp2
  real(rtype) :: eval, pmax, eps2, eps4, &
                 H, q2, rhop, rhoT, hT, theta, u, v, cs, gam
  real(rtype) :: wvec(5), wpvec(5), pmat1(5,5), gmat(5,5), dmat(5,5), &
                 fd(4), fdp(4)

! *****************************************************************************
! i-direction -----------------------------------------------------------------

  do j=2,j2

! - pressure sensor (divided second differences)

    do i=1,il
      dp(i) = Abs((dv(1,i+1,j)-2.D0*dv(1,i,j)+dv(1,i-1,j))/ &
                  (dv(1,i+1,j)+2.D0*dv(1,i,j)+dv(1,i-1,j)))
    enddo

! - dissipation fluxes (at I+1/2)

    do i=1,i2
      im1   = i - 1
      ip1   = i + 1
      ip2   = i + 2
      eval  = 0.5D0*(sri(i,j)+sri(ip1,j)+srj(i,j)+srj(ip1,j))
      pmax  = Max(dp(i),dp(ip1))
      eps2  = eval*vis2*pmax
      eps4  = eval*vis4
      eps4  = Dim(eps4,eps2)
      fd(1) = eps2*(cv(1,ip1,j)-cv(1,i,j)) - &
              eps4*(cv(1,ip2,j)-3.D0*(cv(1,ip1,j)-cv(1,i,j))-cv(1,im1,j))
      fd(2) = eps2*(cv(2,ip1,j)-cv(2,i,j)) - &
              eps4*(cv(2,ip2,j)-3.D0*(cv(2,ip1,j)-cv(2,i,j))-cv(2,im1,j))
      fd(3) = eps2*(cv(3,ip1,j)-cv(3,i,j)) - &
              eps4*(cv(3,ip2,j)-3.D0*(cv(3,ip1,j)-cv(3,i,j))-cv(3,im1,j))
      fd(4) = eps2*(cv(4,ip1,j)-cv(4,i,j)) - &
              eps4*(cv(4,ip2,j)-3.D0*(cv(4,ip1,j)-cv(4,i,j))-cv(4,im1,j))

! --- preconditioning

      rhop  =  0.5D0*(cv(1,i,j)/dv(1,i,j)+cv(1,ip1,j)/dv(1,ip1,j))
      rhoT  = -0.5D0*(cv(1,i,j)/dv(2,i,j)+cv(1,ip1,j)/dv(2,ip1,j))
      hT    = 0.5D0*(dv(5,i,j)+dv(5,ip1,j))
      gam   = 0.5D0*(dv(4,i,j)+dv(4,ip1,j))
      u     = 0.5D0*(cv(2,i,j)/cv(1,i,j)+cv(2,ip1,j)/cv(1,ip1,j))
      v     = 0.5D0*(cv(3,i,j)/cv(1,i,j)+cv(3,ip1,j)/cv(1,ip1,j))
      q2    = u*u + v*v
      H     = 0.5D0*((cv(4,i  ,j)+dv(1,i  ,j))/cv(1,i  ,j)+ &
                     (cv(4,ip1,j)+dv(1,ip1,j))/cv(1,ip1,j))
      cs    = 0.5D0*(dv(3,i,j)+dv(3,ip1,j))
      theta = CompTheta( gam,cs,q2 )

      wvec(1)  = 0.5D0*(cv(1,i,j)+cv(1,ip1,j))
      wvec(2)  = 0.5D0*(cv(2,i,j)+cv(2,ip1,j))
      wvec(3)  = 0.5D0*(cv(3,i,j)+cv(3,ip1,j))
      wvec(4)  = 0.D0
      wvec(5)  = 0.5D0*(cv(4,i,j)+cv(4,ip1,j))
      wpvec(1) = 0.5D0*(dv(1,i,j)+dv(1,ip1,j))
      wpvec(2) = u
      wpvec(3) = v
      wpvec(4) = 0.D0
      wpvec(5) = 0.5D0*(dv(2,i,j)+dv(2,ip1,j))

      call Cons2Prim( wvec,wpvec,H,q2,rhop,rhoT,0.D0,hT,pmat1 )
      call Prim2Cons( wvec,wpvec,H,theta,rhoT,0.D0,hT,gmat )
      call MatrixTimesInverse( wpvec,q2,gmat,pmat1,dmat )
      fdp(1) = dmat(1,1)*fd(1) + dmat(1,2)*fd(2) + &
               dmat(1,3)*fd(3) + dmat(1,5)*fd(4)
      fdp(2) = dmat(2,1)*fd(1) + dmat(2,2)*fd(2) + &
               dmat(2,3)*fd(3) + dmat(2,5)*fd(4)
      fdp(3) = dmat(3,1)*fd(1) + dmat(3,2)*fd(2) + &
               dmat(3,3)*fd(3) + dmat(3,5)*fd(4)
      fdp(4) = dmat(5,1)*fd(1) + dmat(5,2)*fd(2) + &
               dmat(5,3)*fd(3) + dmat(5,5)*fd(4)

! --- dissipation term

      diss(1,i  ,j) = diss(1,i  ,j) + beta*fdp(1)
      diss(2,i  ,j) = diss(2,i  ,j) + beta*fdp(2)
      diss(3,i  ,j) = diss(3,i  ,j) + beta*fdp(3)
      diss(4,i  ,j) = diss(4,i  ,j) + beta*fdp(4)

      diss(1,ip1,j) = diss(1,ip1,j) - beta*fdp(1)
      diss(2,ip1,j) = diss(2,ip1,j) - beta*fdp(2)
      diss(3,ip1,j) = diss(3,ip1,j) - beta*fdp(3)
      diss(4,ip1,j) = diss(4,ip1,j) - beta*fdp(4)
    enddo

  enddo

! j-direction -----------------------------------------------------------------

  do i=2,i2

! - pressure sensor (divided second differences)

    do j=1,jl
      dp(j) = Abs((dv(1,i,j+1)-2.D0*dv(1,i,j)+dv(1,i,j-1))/ &
                  (dv(1,i,j+1)+2.D0*dv(1,i,j)+dv(1,i,j-1)))
    enddo

! - dissipation fluxes (at J+1/2)

    do j=1,j2
      jm1   = j - 1
      jp1   = j + 1
      jp2   = j + 2
      eval  = 0.5D0*(sri(i,j)+sri(i,jp1)+srj(i,j)+srj(i,jp1))
      pmax  = Max(dp(j),dp(jp1))
      eps2  = eval*vis2*pmax
      eps4  = eval*vis4
      eps4  = Dim(eps4,eps2)
      fd(1) = eps2*(cv(1,i,jp1)-cv(1,i,j)) - &
              eps4*(cv(1,i,jp2)-3.D0*(cv(1,i,jp1)-cv(1,i,j))-cv(1,i,jm1))
      fd(2) = eps2*(cv(2,i,jp1)-cv(2,i,j)) - &
              eps4*(cv(2,i,jp2)-3.D0*(cv(2,i,jp1)-cv(2,i,j))-cv(2,i,jm1))
      fd(3) = eps2*(cv(3,i,jp1)-cv(3,i,j)) - &
              eps4*(cv(3,i,jp2)-3.D0*(cv(3,i,jp1)-cv(3,i,j))-cv(3,i,jm1))
      fd(4) = eps2*(cv(4,i,jp1)-cv(4,i,j)) - &
              eps4*(cv(4,i,jp2)-3.D0*(cv(4,i,jp1)-cv(4,i,j))-cv(4,i,jm1))

! --- preconditioning

      rhop  =  0.5D0*(cv(1,i,j)/dv(1,i,j)+cv(1,i,jp1)/dv(1,i,jp1))
      rhoT  = -0.5D0*(cv(1,i,j)/dv(2,i,j)+cv(1,i,jp1)/dv(2,i,jp1))
      hT    = 0.5D0*(dv(5,i,j)+dv(5,i,jp1))
      gam   = 0.5D0*(dv(4,i,j)+dv(4,i,jp1))
      u     = 0.5D0*(cv(2,i,j)/cv(1,i,j)+cv(2,i,jp1)/cv(1,i,jp1))
      v     = 0.5D0*(cv(3,i,j)/cv(1,i,j)+cv(3,i,jp1)/cv(1,i,jp1))
      q2    = u*u + v*v
      H     = 0.5D0*((cv(4,i,j  )+dv(1,i,j  ))/cv(1,i,j  )+ &
                     (cv(4,i,jp1)+dv(1,i,jp1))/cv(1,i,jp1))
      cs    = 0.5D0*(dv(3,i,j)+dv(3,i,jp1))
      theta = CompTheta( gam,cs,q2 )

      wvec(1)  = 0.5D0*(cv(1,i,j)+cv(1,i,jp1))
      wvec(2)  = 0.5D0*(cv(2,i,j)+cv(2,i,jp1))
      wvec(3)  = 0.5D0*(cv(3,i,j)+cv(3,i,jp1))
      wvec(4)  = 0.D0
      wvec(5)  = 0.5D0*(cv(4,i,j)+cv(4,i,jp1))
      wpvec(1) = 0.5D0*(dv(1,i,j)+dv(1,i,jp1))
      wpvec(2) = u
      wpvec(3) = v
      wpvec(4) = 0.D0
      wpvec(5) = 0.5D0*(dv(2,i,j)+dv(2,i,jp1))

      call Cons2Prim( wvec,wpvec,H,q2,rhop,rhoT,0.D0,hT,pmat1 )
      call Prim2Cons( wvec,wpvec,H,theta,rhoT,0.D0,hT,gmat )
      call MatrixTimesInverse( wpvec,q2,gmat,pmat1,dmat )
      fdp(1) = dmat(1,1)*fd(1) + dmat(1,2)*fd(2) + &
               dmat(1,3)*fd(3) + dmat(1,5)*fd(4)
      fdp(2) = dmat(2,1)*fd(1) + dmat(2,2)*fd(2) + &
               dmat(2,3)*fd(3) + dmat(2,5)*fd(4)
      fdp(3) = dmat(3,1)*fd(1) + dmat(3,2)*fd(2) + &
               dmat(3,3)*fd(3) + dmat(3,5)*fd(4)
      fdp(4) = dmat(5,1)*fd(1) + dmat(5,2)*fd(2) + &
               dmat(5,3)*fd(3) + dmat(5,5)*fd(4)

! --- dissipation term

      diss(1,i,j  ) = diss(1,i,j  ) + beta*fdp(1)
      diss(2,i,j  ) = diss(2,i,j  ) + beta*fdp(2)
      diss(3,i,j  ) = diss(3,i,j  ) + beta*fdp(3)
      diss(4,i,j  ) = diss(4,i,j  ) + beta*fdp(4)

      diss(1,i,jp1) = diss(1,i,jp1) - beta*fdp(1)
      diss(2,i,jp1) = diss(2,i,jp1) - beta*fdp(2)
      diss(3,i,jp1) = diss(3,i,jp1) - beta*fdp(3)
      diss(4,i,jp1) = diss(4,i,jp1) - beta*fdp(4)
    enddo

  enddo

end subroutine DissipCentralPrec

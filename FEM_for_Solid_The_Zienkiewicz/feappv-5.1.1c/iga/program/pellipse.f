!$Id:$
      subroutine pellipse(xx,ndisp, u,v, psflag)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  2-d  user displacements for circlular hole NURBS

!      Inputs:
!        xx(ndm)     - Coordinates at quadrature point
!        ndisp(3)    - Load parameters 1, 2 and 3
!        psflag      - Flag for plane strain(T)/stress(F)

!      Outputs:
!        u,v         - x & y displacements
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      logical          :: psflag

      real    (kind=8) :: x,y,e,nu,a,b,kappa, mu,alpha,p, u,v
      real    (kind=8) :: xx(2),ndisp(3)
      complex (kind=8) :: z,zeta, phi, psi, d_phi, beta, zalpha

      real    (kind=8) :: r,m

      e     = 1.0d5
      nu    = 0.3d0
      mu    = e/(2.0d0*(1.d0 + nu))
      alpha = 0.0d0

!     Plane strain
      if(psflag) then

        kappa = 3.0d0 - 4.0d0*nu

!     Plane stress
      else

        kappa = (3.0d0 - nu)/(1.0d0 + nu)

      endif

!     set traction
      p = ndisp(1)  ! Traction at infinity
      a = ndisp(2)  ! x-axis of hole ellipse
      b = ndisp(3)  ! y-axis of hole ellipse

      x = xx(1)
      y = xx(2)

      r = (a + b)*0.5d0
      m = (a - b)/(a + b)

      z      = dcmplx(x,y)
      zeta   = 0.5d0/(m*r)*(z - zsqrt(z*z - 4.0d0*m*r**2))
      zalpha = dcmplx(0.0d0,2.d0*alpha)

!     Stress functions
!     phi  =  0.25d0*p*r*((2.d0*zexp(zalpha)-m)*zeta + 1.d0/zeta)
!     psi  = -0.5d0*p*r*(zexp(-zalpha)/zeta + zexp(zalpha)*zeta/m
!    &     - ((1.d0+m**2)*(zexp(zalpha)-m)/m)*zeta/(1.d0-m*zeta*zeta))

      phi  =  0.25d0*p*r*((2.d0-m)*zeta + 1.d0/zeta)
      psi  = -0.50d0*p*r*(1.0d0/zeta + zeta/m
     &     - ((1.d0+m**2)*(1.0d0-m)/m)*zeta/(1.d0-m*zeta**2))
!     Derivative
!     d_phi = 0.25d0*p*(2.d0*(zexp(zalpha)-m)/(m-1.d0/(zeta*zeta))+1.d0)

      d_phi = 0.25d0*p*(2.d0*(1.0d0 - m)/(m - 1.d0/zeta**2) + 1.d0)
!     Displacements
      beta = kappa*phi - z*conjg(d_phi) - conjg(psi)
      u    = 0.5d0/mu*real(beta)
      v    = 0.5d0/mu*imag(beta)

      end

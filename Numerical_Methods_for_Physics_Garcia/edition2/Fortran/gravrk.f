      subroutine gravrk( x, t, param, deriv )
      real*8 x(*), t, param(*), deriv(*)
!  Returns right-hand side of Kepler ODE; used by Runge-Kutta routines
!  Inputs
!    x      State vector [r(1) r(2) v(1) v(2)]
!    t      Time (not used)
!    param     Parameter G*M (gravitational const. * solar mass)
!  Output
!    deriv  Derivatives [dr(1)/dt dr(2)/dt dv(1)/dt dv(2)/dt]
      real*8 GM, r1, r2, v1, v2, normR, accel1, accel2

      !* Compute acceleration
      GM = param(1)
      r1 = x(1)
      r2 = x(2)     ! Unravel the vector x into
      v1 = x(3)     ! position and velocity
      v2 = x(4)
      normR = sqrt( r1*r1 + r2*r2 )
      accel1 = -GM*r1/(normR**3)  ! Gravitational acceleration
      accel2 = -GM*r2/(normR**3)

      !* Return derivatives [dr(1)/dt dr(2)/dt dv(1)/dt dv(2)/dt]
      deriv(1) = v1
      deriv(2) = v2
      deriv(3) = accel1
      deriv(4) = accel2
      return
      end

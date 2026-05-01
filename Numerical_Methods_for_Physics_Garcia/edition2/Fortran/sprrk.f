      subroutine sprrk( x, t, param, deriv )
      real*8 x(*), t, param(*), deriv(*)
!  Returns right-hand side of 3 mass-spring system
!  equations of motion
!  Inputs
!    x       State vector [x(1) x(2) ... v(3)]
!    t       Time (not used)
!    param   (Spring constant)/(Block mass)
!  Output
!    deriv   [dx(1)/dt dx(2)/dt ... dv(3)/dt]

      real*8 param2

      deriv(1) = x(4)
      deriv(2) = x(5)
      deriv(3) = x(6)
      param2 = -2*param(1)
      deriv(4) = param2*x(1) + param(1)*x(2)
      deriv(5) = param2*x(2) + param(1)*(x(1)+x(3))
      deriv(6) = param2*x(3) + param(1)*x(2)
      return
      end

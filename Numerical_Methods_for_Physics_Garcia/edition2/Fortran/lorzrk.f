      subroutine lorzrk( xx, t, param, deriv )
      real*8 xx(*), t, param(*), deriv(*)
!  Returns right-hand side of Lorenz model ODEs
!  Inputs
!    X      State vector [x y z]
!    t      Time (not used)
!    param  Parameters [r sigma b]
!  Output
!    deriv  Derivatives [dx/dt dy/dt dz/dt]

      real*8 x, y, z, r, sigma, b

      !* For clarity, unravel input vectors
      x = xx(1)
      y = xx(2)
      z = xx(3)
      r = param(1)
      sigma = param(2)
      b = param(3)

      !* Return the derivatives [dx/dt dy/dt dz/dt]
      deriv(1) = sigma*(y-x)
      deriv(2) = r*x - y - x*z
      deriv(3) = x*y - b*z
      return
      end

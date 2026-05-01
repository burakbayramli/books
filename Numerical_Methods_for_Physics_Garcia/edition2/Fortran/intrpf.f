      real*8 function intrpf( xi, x, y )
! Function to interpolate between data points
! using Lagrange polynomial (quadratic)
! Inputs
!   xi   The x value where interpolation is computed
!   x    Vector of x coordinates of data points (3 values)
!   y    Vector of y coordinates of data points (3 values)
! Output
!   yi   The interpolation polynomial evaluated at xi
      real*8 xi, x(3), y(3), yi

  !* Calculate yi = p(xi) using Lagrange polynomial
      yi = (xi-x(2))*(xi-x(3))/((x(1)-x(2))*(x(1)-x(3)))*y(1)
     &   + (xi-x(1))*(xi-x(3))/((x(2)-x(1))*(x(2)-x(3)))*y(2)
     &   + (xi-x(1))*(xi-x(2))/((x(3)-x(1))*(x(3)-x(2)))*y(3)
      intrpf = yi
      return
      end

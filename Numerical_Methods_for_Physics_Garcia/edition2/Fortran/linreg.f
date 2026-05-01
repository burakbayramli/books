      subroutine linreg( x, y, sigma, N, a_fit, sig_a, yy, chisqr )
      integer*4 N
      real*8 x(N), y(N), sigma(N), a_fit(2), sig_a(2)
      real*8 yy(N), chisqr
! Function to perform linear regression (fit a line)
! Inputs
!   x       Independent variable
!   y       Dependent variable
!   sigma   Estimated error in y
!   N       Number of data points
! Outputs
!   a_fit   Fit parameters; a(1) is intercept, a(2) is slope
!   sig_a   Estimated error in the parameters a()
!   yy      Curve fit to the data
!   chisqr  Chi squared statistic

      integer*4 i
      real*8 sigmaTerm, s, sx, sy, sxy, sxx, denom, delta

      !* Evaluate various sigma sums
      s = 0.0
      sx = 0.0
      sy = 0.0
      sxy = 0.0
      sxx = 0.0
      do i=1,N
        sigmaTerm = 1.0/sigma(i)**2
        s = s + sigmaTerm
        sx = sx + x(i) * sigmaTerm
        sy = sy + y(i) * sigmaTerm
        sxy = sxy + x(i) * y(i) * sigmaTerm
        sxx = sxx + x(i) * x(i) * sigmaTerm
      enddo
      denom = s*sxx - sx*sx

      !* Compute intercept a_fit(1) and slope a_fit(2)
      a_fit(1) = (sxx*sy - sx*sxy)/denom
      a_fit(2) = (s*sxy - sx*sy)/denom

      !* Compute error bars for intercept and slope
      sig_a(1) = sqrt(sxx/denom)
      sig_a(2) = sqrt(s/denom)

      !* Evaluate curve fit at each data point and compute Chi^2
      chisqr = 0.0
      do i=1,N
        yy(i) = a_fit(1)+a_fit(2)*x(i)     ! Curve fit to the data
        delta = (y(i)-yy(i))/sigma(i)
        chisqr = chisqr + delta**2         ! Chi square
      enddo
      return
      end

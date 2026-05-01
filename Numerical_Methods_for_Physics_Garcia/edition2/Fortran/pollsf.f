      subroutine pollsf( x, y, sigma, N, M, a_fit, sig_a, yy, chisqr )
      integer*4 MAXN, MAXM
      parameter( MAXN = 10000, MAXM = 50 )
      integer*4 N, M
      real*8 x(N), y(N), sigma(N), a_fit(M), sig_a(M)
      real*8 yy(N), chisqr
! Function to fit a polynomial to data
! Inputs
!   x       Independent variable
!   y       Dependent variable
!   sigma   Estimate error in y
!   M       Number of parameters used to fit data
! Outputs
!   a_fit   Fit parameters; a(1) is intercept, a(2) is slope
!   sig_a   Estimated error in the parameters a()
!   yy      Curve fit to the data
!   chisqr  Chi squared statistic

      integer*4 i, j, k
      real*8 b(MAXN), A(MAXN,MAXM), C(MAXM,MAXM), Cinv(MAXM,MAXM)
      real*8 delta, determ, inv

      !* Form the vector b and design matrix A
      do i=1,N
       b(i) = y(i)/sigma(i)
       do j=1,M
         A(i,j) = (x(i)**(j-1))/sigma(i)
       enddo
      enddo

      !* Compute the correlation matrix C
      do i=1,M                  ! (C inverse) = (A transpose) * A
        do j=1,M
          Cinv(i,j) = 0.0
          do k=1,N
            Cinv(i,j) = Cinv(i,j) + A(k,i)*A(k,j)
          enddo
        enddo
      enddo
      ! C = ( (C inverse) inverse)
      determ = inv( Cinv, M, MAXM, C )  ! Determinant returned but unused

      !* Compute the least squares polynomial coefficients a_fit
      do k=1,M
        a_fit(k) = 0.0
        do j=1,M
         do i=1,N
           a_fit(k) = a_fit(k) + C(k,j) * A(i,j) * b(i)
         enddo
        enddo
      enddo

      !* Compute the estimated error bars for the coefficients
      do j=1,M
        sig_a(j) = sqrt(C(j,j))
      enddo

      !* Evaluate curve fit at each data point and compute Chi^2
      chisqr = 0.0
      do i=1,N
        yy(i) = 0.0       ! yy is the curve fit
        do j=1,M
          yy(i) = yy(i) + a_fit(j) * x(i)**(j-1)
        enddo
        delta = (y(i)-yy(i))/sigma(i)
        chisqr = chisqr + delta**2  ! Chi square
      enddo
      return
      end

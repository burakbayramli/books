! lsfdemo - Program for demonstrating least squares fit routines

      program lsfdemo
      integer*4 MAXN, MAXM
      parameter( MAXN = 10000, MAXM = 50 )
      integer*4 i, N, seed, M
      real*8 c(3), alpha, x(MAXN), y(MAXN), sigma(MAXN)
      real*8 a_fit(MAXM), sig_a(MAXM), yy(MAXN), chisqr
      real*8 randn

      !* Initialize data to be fit. Data is quadratic plus random number.
      write(*,*) 'Curve fit data is created using the quadratic'
      write(*,*) '  y(x) = c(1) + c(2)*x + c(3)*x^2'
      write(*,*) 'Enter the coefficients:'
      write(*,*) 'c(1), c(2), c(3) = '
      read(*,*) c(1), c(2), c(3)
      write(*,*) 'Enter estimated error bar: '
      read(*,*) alpha
      N = 50          ! Number of data points
      seed = 1234     ! Seed for random number generator (DO NOT USE ZERO)
      do i=1,N
        x(i) = i                  ! x = [1, 2, ..., N]
        y(i) = c(1) + c(2)*x(i) + c(3)*x(i)**2 + alpha*randn(seed)
        sigma(i) = alpha          ! Constant error bar
      enddo

      !* Fit the data to a straight line or a more general polynomial
      write(*,*) 'Enter number of fit parameters (=2 for line): '
      read(*,*) M
      if( M .eq. 2 ) then  !* Linear regression (Straight line) fit
        call linreg( x, y, sigma, N, a_fit, sig_a, yy, chisqr)
      else          !* Polynomial fit
        call pollsf( x, y, sigma, N, M, a_fit, sig_a, yy, chisqr)
      endif

      !* Print out the fit parameters, including their error bars.
      write(*,*) 'Fit parameters:'
      do i=1,M
        write(*,*) ' a(', i, ') = ', a_fit(i), ' +/- ', sig_a(i)
      enddo
      write(*,*) 'Chi square = ', chisqr, '; N-M = ', N-M

      !* Print out the plotting variables: x, y, sigma, yy
      open(11,file='x.txt',status='unknown')
      open(12,file='y.txt',status='unknown')
      open(13,file='sigma.txt',status='unknown')
      open(14,file='yy.txt',status='unknown')
      do i=1,N
        write(11,*) x(i)
        write(12,*) y(i)
        write(13,*) sigma(i)
        write(14,*) yy(i)
      enddo
      stop
      end

!***** To plot in MATLAB; use the script below ********************
!load x.txt; load y.txt; load sigma.txt; load yy.txt
!%* Graph the data, with error bars, and fitting function.
!figure(1); clf;           % Bring figure 1 window forward
!errorbar(x,y,sigma,'o');  % Graph data with error bars
!hold on;                  % Freeze the plot to add the fit
!plot(x,yy,'-');           % Plot the fit on same graph as data
!xlabel('x_i'); ylabel('y_i and Y(x)');
!******************************************************************

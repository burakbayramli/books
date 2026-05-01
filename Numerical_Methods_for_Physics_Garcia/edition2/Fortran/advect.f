! advect - Program to solve the advection equation
! using the various hyperbolic PDE schemes

      program advect
      integer*4 MAXN, MAXnplots
      parameter( MAXN = 500, MAXnplots = 500 )
      integer*4 method, N, nStep, i, j, ip(MAXN), im(MAXN)
      integer*4 iplot, nplots, iStep
      real*8 L, h, c, tau, coeff, coefflw, pi, sigma, k_wave
      real*8 x(MAXN), a(MAXN), a_new(MAXN), plotStep
      real*8 aplot(MAXN,MAXnplots+1), tplot(MAXnplots+1)

      !* Select numerical parameters (time step, grid spacing, etc.).
      write(*,*) 'Choose a numerical method: '
      write(*,*) '   1) FTCS, 2) Lax, 3) Lax-Wendroff : '
      read(*,*) method
      write(*,*) 'Enter number of grid points: '
      read(*,*) N
      L = 1.      ! System size
      h = L/N     ! Grid spacing
      c = 1       ! Wave speed
      write(*,*) 'Time for wave to move one grid spacing is ', h/c
      write(*,*) 'Enter time step: '
      read(*,*) tau
      coeff = -c*tau/(2.*h)     ! Coefficient used by all schemes
      coefflw = 2*coeff*coeff   ! Coefficient used by L-W scheme
      write(*,*) 'Wave circles system in ', L/(c*tau), ' steps'
      write(*,*) 'Enter number of steps: '
      read(*,*) nStep

      !* Set initial and boundary conditions.
      pi = 3.141592654
      sigma = 0.1              ! Width of the Gaussian pulse
      k_wave = pi/sigma        ! Wave number of the cosine
      do i=1,N
        x(i) = (i-0.5)*h - L/2  ! Coordinates of grid points
        ! Initial condition is a Gaussian-cosine pulse
        a(i) = cos(k_wave*x(i)) * exp(-x(i)**2/(2*sigma**2))
      enddo
      ! Use periodic boundary conditions
      do i=2,(N-1)
        ip(i) = i+1    ! ip(i) = i+1 with periodic b.c.
        im(i) = i-1    ! im(i) = i-1 with periodic b.c.
      enddo
      ip(1) = 2
      ip(N) = 1
      im(1) = N
      im(N) = N-1

      !* Initialize plotting variables.
      iplot = 1          ! Plot counter
      nplots = 50        ! Desired number of plots
      plotStep = float(nStep)/nplots
      tplot(1) = 0       ! Record the initial time (t=0)
      do i=1,N
        aplot(i,1) = a(i)  ! Record the initial state
      enddo

      !* Loop over desired number of steps.
      do iStep=1,nStep

        !* Compute new values of wave amplitude using FTCS,
        !  Lax or Lax-Wendroff method.
        if( method .eq. 1 ) then       !!! FTCS method !!!
          do i=1,N
            a_new(i) = a(i) + coeff*( a(ip(i))-a(im(i)) )
          enddo
        else if( method .eq. 2 ) then   !!! Lax method !!!
          do i=1,N
            a_new(i) = 0.5*( a(ip(i))+a(im(i)) ) +
     &                  coeff*( a(ip(i))-a(im(i)) )
          enddo
        else                   !!! Lax-Wendroff method !!!
          do i=1,N
            a_new(i) = a(i) + coeff*( a(ip(i))-a(im(i)) ) +
     &                        coefflw*( a(ip(i))+a(im(i))-2*a(i) )
          enddo
        endif

        do i=1,N
          a(i) = a_new(i)   ! Reset with new amplitude values
        enddo

        !* Periodically record a(t) for plotting.
        if( (iStep-int(iStep/plotStep)*plotStep) .lt. 1 ) then
          iplot = iplot+1
          tplot(iplot) = tau*iStep
          do i=1,N
            aplot(i,iplot) = a(i)       ! Record a(i) for ploting
          enddo
          write(*,*) iStep, ' out of ', nStep, ' steps completed'
        endif
      enddo
      nplots = iplot   ! Actual number of plots recorded

      !* Print out the plotting variables: x, a, tplot, aplot
      open(11,file='x.txt',status='unknown')
      open(12,file='a.txt',status='unknown')
      open(13,file='tplot.txt',status='unknown')
      open(14,file='aplot.txt',status='unknown')
      do i=1,N
        write(11,*) x(i)
        write(12,*) a(i)
        do j=1,(nplots-1)
          write(14,1001) aplot(i,j)
        enddo
        write(14,*) aplot(i,nplots)
      enddo
      do i=1,nplots
        write(13,*) tplot(i)
      enddo
1001  format(e12.6,', ',$)   ! The $ suppresses the carriage return

      stop
      end

!***** To plot in MATLAB; use the script below ********************
!load x.txt; load a.txt; load tplot.txt; load aplot.txt;
!%* Plot the initial and final states.
!figure(1); clf;  % Clear figure 1 window and bring forward
!plot(x,aplot(:,1),'-',x,a,'--');
!legend('Initial','Final');
!xlabel('x');  ylabel('a(x,t)');
!pause(1);    % Pause 1 second between plots
!%* Plot the wave amplitude versus position and time
!figure(2); clf;  % Clear figure 2 window and bring forward
!mesh(tplot,x,aplot);
!ylabel('Position');  xlabel('Time'); zlabel('Amplitude');
!view([-70 50]);  % Better view from this angle
!******************************************************************

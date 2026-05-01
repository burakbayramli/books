! traffic - Program to solve the generalized Burger
! equation for the traffic at a stop light problem

      program traffic
      integer*4 MAXN, MAXnStep
      parameter( MAXN = 200, MAXnStep = 1000 )
      integer*4 method, N, i, j, iFront, iBack, iplot, iStep, nStep
      integer*4 ip(MAXN+1), im(MAXN+1), nplots
      real*8 L, h, v_max, coeff, coefflw, cp, cm, rho_max, Flow_max
      real*8 rho(MAXN), rho_new(MAXN), Flow(MAXN), tau
      real*8 tplot(MAXnStep+1), xplot(MAXN), rplot(MAXN,MAXnStep+1)

      !* Select numerical parameters (time step, grid spacing, etc.).
      write(*,*) 'Choose a numerical method: '
      write(*,*) '   1) FTCS, 2) Lax, 3) Lax-Wendroff : '
      read(*,*) method
      write(*,*) 'Enter the number of grid points: '
      read(*,*) N
      L = 400       ! System size (meters)
      h = L/N       ! Grid spacing for periodic boundary conditions
      v_max = 25    ! Maximum car speed (m/s)
      write(*,*) 'Suggested timestep is ', h/v_max
      write(*,*) 'Enter time step (tau): '
      read(*,*) tau
      write(*,*) 'Last car starts moving after ',
     &                  (L/4)/(v_max*tau), ' steps'
      write(*,*) 'Enter number of steps: '
      read(*,*) nStep
      coeff = tau/(2*h)           ! Coefficient used by all schemes
      coefflw = tau**2/(2*h**2)   ! Coefficient used by Lax-Wendroff

      !* Set initial and boundary conditions
      rho_max = 1.0                   ! Maximum density
      Flow_max = 0.25*rho_max*v_max   ! Maximum Flow
      ! Initial condition is a square pulse from x = -L/4 to x = 0
      iBack = N/4
      iFront = N/2 - 1
      do i=1,N
        if( iBack .le. i .and. i .le. iFront ) then
          rho(i) = rho_max
        else
          rho(i) = 0.0
        endif
      enddo
      rho(iFront+1) = rho_max/2   ! Try running without this line
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
      iplot = 1
      tplot(1) = 0.0             ! Record initial time
      do i=1,N
        xplot(i) = (i - 0.5)*h - L/2   ! Record x scale for plot
        rplot(i,1) = rho(i)            ! Record the initial state
      enddo

      !* Loop over desired number of steps.
      do iStep=1,nStep

        !* Compute the flow = (Density)*(Velocity)
        do i=1,N
          Flow(i) = rho(i) * (v_max*(1.0 - rho(i)/rho_max))
        enddo

        !* Compute new values of density using FTCS,
        !  Lax or Lax-Wendroff method.
        if( method .eq. 1 ) then       !!! FTCS method !!!
          do i=1,N
            rho_new(i) = rho(i) - coeff*(Flow(ip(i))-Flow(im(i)))
          enddo
        else if( method .eq. 2 ) then  !!! Lax method !!!
          do i=1,N
            rho_new(i) = 0.5*(rho(ip(i))+rho(im(i)))
     &                 - coeff*(Flow(ip(i))-Flow(im(i)))
          enddo
        else                           !!! Lax-Wendroff method !!!
          do i=1,N
            cp = v_max*(1 - (rho(ip(i))+rho(i))/rho_max)
            cm = v_max*(1 - (rho(i)+rho(im(i)))/rho_max)
            rho_new(i) = rho(i) - coeff*(Flow(ip(i))-Flow(im(i)))
     &                 + coefflw*(cp*(Flow(ip(i))-Flow(i))
     &                          - cm*(Flow(i)-Flow(im(i))))
          enddo
        endif
        ! Reset with new density values
        do i=1,N
          rho(i) = rho_new(i)
        enddo

        !* Record density for plotting.
        write(*,*) 'Finished ', iStep, ' of ', nStep, ' steps'
        iplot = iplot+1
        tplot(iplot) = tau*iStep
        do i=1,N
          rplot(i,iplot) = rho(i)
        enddo
      enddo
      nplots = iplot     ! Number of plots recorded

      !* Print out the plotting variables: tplot, xplot, rplot
      open(11,file='tplot.txt',status='unknown')
      open(12,file='xplot.txt',status='unknown')
      open(13,file='rplot.txt',status='unknown')
      do i=1,nplots
        write(11,*) tplot(i)
      enddo
      do i=1,N
        write(12,*) xplot(i)
        do j=1,(nplots-1)
          write(13,1001) rplot(i,j)
        enddo
        write(13,*) rplot(i,nplots)
      enddo
1001  format(e12.6,', ',$)   ! The $ suppresses the carriage return

      stop
      end
!***** To plot in MATLAB; use the script below ********************
!load tplot.txt; load xplot.txt; load rplot.txt;
!%* Graph density versus position and time as wire-mesh plot
!figure(1); clf;  % Clear figure 1 window and bring forward
!mesh(tplot,xplot,rplot)
!xlabel('t'); ylabel('x'); zlabel('\rho');
!title('Density versus position and time');
!view([100 30]);  % Rotate the plot for better view point
!pause(1);    % Pause 1 second between plots
!%* Graph contours of density versus position and time.
!figure(2); clf;   % Clear figure 2 window and bring forward
!% Use rot90 function to graph t vs x since
!% contour(rplot) graphs x vs t.
!clevels = 0:(0.1):1;   % Contour levels
!cs = contour(xplot,tplot,flipud(rot90(rplot)),clevels);
!clabel(cs);            % Put labels on contour levels
!xlabel('x');  ylabel('time');  title('Density contours');
!******************************************************************

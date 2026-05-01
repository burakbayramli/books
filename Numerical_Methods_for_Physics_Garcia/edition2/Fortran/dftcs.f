! dftcs - Program to solve the diffusion equation
! using the Forward Time Centered Space (FTCS) scheme.

      program dftcs
      integer*4 MAXN, MAXnplots
      parameter( MAXN = 300, MAXnplots = 500 )
      integer*4 N, i, j, iplot, nStep, plot_step, nplots, iStep
      real*8 tau, L, h, kappa, coeff, tt(MAXN), tt_new(MAXN)
      real*8 xplot(MAXN), tplot(MAXnplots), ttplot(MAXN,MAXnplots)

      !* Initialize parameters (time step, grid spacing, etc.).
      write(*,*) 'Enter time step: '
      read(*,*) tau
      write(*,*) 'Enter the number of grid points: '
      read(*,*) N
      L = 1.               ! The system extends from x=-L/2 to x=L/2
      h = L/(N-1)          ! Grid size
      kappa = 1.           ! Diffusion coefficient
      coeff = kappa*tau/h**2
      if( coeff .lt. 0.5 ) then
        write(*,*) 'Solution is expected to be stable'
      else
        write(*,*) 'WARNING: Solution is expected to be unstable'
      endif

      !* Set initial and boundary conditions.
      do i=1,N
        tt(i) = 0.0    ! Initialize temperature to zero at all points
        tt_new(i) = 0.0
      enddo
      tt(N/2) = 1/h    ! Initial cond. is delta function in center
      !! The boundary conditions are tt(1) = tt(N) = 0
      !! End points are unchanged during iteration

      !* Set up loop and plot variables.
      iplot = 1                 ! Counter used to count plots
      nStep = 300               ! Maximum number of iterations
      plot_step = 6             ! Number of time steps between plots
      nplots = nStep/plot_step + 1  ! Number of snapshots (plots)
      do i=1,N
        xplot(i) = (i-1)*h - L/2   ! Record the x scale for plots
      enddo

      !* Loop over the desired number of time steps.
      do iStep=1,nStep

        !* Compute new temperature using FTCS scheme.
        do i=2,(N-1)
          tt_new(i) = tt(i) + coeff*(tt(i+1) + tt(i-1) - 2*tt(i))
        enddo
        do i=2,(N-1)
          tt(i) = tt_new(i)     ! Reset temperature to new values
        enddo

        !* Periodically record temperature for plotting.
        if( mod(iStep,plot_step) .lt. 1 ) then ! Every plot_step steps
          do i=1,N                             ! record tt(i) for plotting
            ttplot(i,iplot) = tt(i)
          enddo
          tplot(iplot) = iStep*tau             ! Record time for plots
          iplot = iplot+1
        endif
      enddo
      nplots = iplot-1   ! Number of plots actually recorded

      !* Print out the plotting variables: tplot, xplot, ttplot
      open(11,file='tplot.txt',status='unknown')
      open(12,file='xplot.txt',status='unknown')
      open(13,file='ttplot.txt',status='unknown')
      do i=1,nplots
        write(11,*) tplot(i)
      enddo
      do i=1,N
        write(12,*) xplot(i)
        do j=1,(nplots-1)
          write(13,1001) ttplot(i,j)
        enddo
        write(13,*) ttplot(i,nplots)
      enddo
1001  format(e12.6,', ',$)  ! The $ suppresses the carriage return
      stop
      end
!***** To plot in MATLAB; use the script below ********************
!load tplot.txt; load xplot.txt; load ttplot.txt;
!%* Plot temperature versus x and t as wire-mesh and contour plots.
!figure(1); clf;
!mesh(tplot,xplot,ttplot);  % Wire-mesh surface plot
!xlabel('Time');  ylabel('x');  zlabel('T(x,t)');
!title('Diffusion of a delta spike');
!pause(1);
!figure(2); clf;
!contourLevels = 0:0.5:10;  contourLabels = 0:5;
!cs = contour(tplot,xplot,ttplot,contourLevels);  % Contour plot
!clabel(cs,contourLabels);  % Add labels to selected contour levels
!xlabel('Time'); ylabel('x');
!title('Temperature contour plot');
!******************************************************************

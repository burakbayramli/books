! neutrn - Program to solve the neutron diffusion equation
! using the Forward Time Centered Space (FTCS) scheme.

      program neutrn
      integer*4 MAXN, MAXnplots
      parameter( MAXN = 500, MAXnplots = 500 )
      integer*4 N, iplot, nplots, plot_step, i, j, iStep, nStep
      real*8 tau, L, h, D, C, coeff, coeff2, nSum
      real*8 nn(MAXN), nn_new(MAXN), nAve(MAXnplots)
      real*8 xplot(MAXN), tplot(MAXnplots), nnplot(MAXN,MAXnplots)

      !* Initialize parameters (time step, grid spacing, etc.).
      write(*,*) 'Enter time step: '
      read(*,*) tau
      write(*,*) 'Enter the number of grid points: '
      read(*,*) N
      write(*,*) 'Enter system length: '
      read(*,*) L
      ! The system extends from x=-L/2 to x=L/2
      h = L/(N-1)  ! Grid size
      D = 1.       ! Diffusion coefficient
      C = 1.       ! Generation rate
      coeff = D*tau/(h*h)
      coeff2 = C*tau
      if( coeff .lt. 0.5 ) then
        write(*,*) 'Solution is expected to be stable'
      else
        write(*,*) 'WARNING: Solution is expected to be unstable'
      endif

      !* Set initial and boundary conditions.
      do i=1,N
        nn(i) = 0.0     ! Initialize density to zero at all points
        nn_new(i) = 0.0
      enddo
      nn(N/2) = 1/h     ! Initial cond. is delta function in center
      !! The boundary conditions are nn(1) = nn(N) = 0
      !! End points are unchanged during iteration

      !* Set up loop and plot variables.
      iplot = 1                 ! Counter used to count plots
      write(*,*) 'Enter number of time steps: '
      read(*,*) nStep
      plot_step = 200           ! Number of time steps between plots
      nplots = nStep/plot_step + 1  ! Number of snapshots (plots)
      do i=1,N
        xplot(i) = (i-1)*h - L/2    ! Record the x scale for plots
      enddo

      !* Loop over the desired number of time steps.
      do iStep=1,nStep

        !* Compute new density using FTCS scheme.
        do i=2,(N-1)
          nn_new(i) = nn(i) + coeff*(nn(i+1) + nn(i-1) - 2*nn(i))
     &                      + coeff2*nn(i)
        enddo
        do i=2,(N-1)
          nn(i) = nn_new(i)     ! Reset density to new values
        enddo

        !* Periodically record density for plotting.
        if( mod(iStep,plot_step) .lt. 1 ) then ! Every plot_step steps ...
          nSum = 0
          do i=1,N
            nnplot(i,iplot) = nn(i)   ! Record tt(i) for plotting
            nSum = nSum + nn(i)
          enddo
          nAve(iplot) = nSum/N
          tplot(iplot) = iStep*tau   ! Record time for plots
          iplot = iplot+1
        endif
      enddo
      nplots = iplot-1   ! Number of plots actually recorded

      !* Print out the plotting variables: tplot, xplot, nnplot, nAve
      open(11,file='tplot.txt',status='unknown')
      open(12,file='xplot.txt',status='unknown')
      open(13,file='nnplot.txt',status='unknown')
      open(14,file='nAve.txt',status='unknown')
      do i=1,nplots
        write(11,*) tplot(i)
        write(14,*) nAve(i)
      enddo
      do i=1,N
        write(12,*) xplot(i)
        do j=1,(nplots-1)
          write(13,1001) nnplot(i,j)
        enddo
        write(13,*) nnplot(i,nplots)
      enddo
1001  format(e12.6,', ',$)   ! The $ suppresses the carriage return
      stop
      end

!***** To plot in MATLAB; use the script below ********************
!load tplot.txt; load xplot.txt; load nnplot.txt; load nAve.txt;
!%* Plot density versus x and t as a 3D-surface plot
!figure(1); clf;
!surf(tplot,xplot,nnplot);
!xlabel('Time');  ylabel('x');  zlabel('n(x,t)');
!title('Neutron diffusion');
!%* Plot average neutron density versus time
!figure(2); clf;
!plot(tplot,nAve,'*');
!xlabel('Time'); ylabel('Average density');
!******************************************************************

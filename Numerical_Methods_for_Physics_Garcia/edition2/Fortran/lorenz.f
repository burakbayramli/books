! lorenz - Program to compute the trajectories of the Lorenz
! equations using the adaptive Runge-Kutta method.

      program lorenz
      integer*4 nState, MAXnStep
      parameter( nState = 3, MAXnStep = 100000 )
      integer*4 iStep, nStep, i
      real*8 x, y, z, state(nState), r, sigma, b, param(3)
      real*8 tau, err, time, maxTau, minTau
      real*8 tplot(MAXnStep), tauplot(MAXnStep)   ! Plotting variables
      real*8 xplot(MAXnStep), yplot(MAXnStep), zplot(MAXnStep)
      real*8 x_ss(3), y_ss(3), z_ss(3)
      external lorzrk

      !* Set initial state x,y,z and parameters r,sigma,b
      write(*,*) 'Enter initial state (x,y,z)'
      write(*,*) 'x, y, z = '
      read(*,*) x, y, z
      state(1) = x
      state(2) = y
      state(3) = z
      write(*,*) 'Enter the parameter r: '
      read(*,*) r
      sigma = 10.   ! Parameter sigma
      b = 8./3.     ! Parameter b
      param(1) = r
      param(2) = sigma
      param(3) = b
      tau = 1.0       ! Initial guess for the timestep
      err = 1.e-3     ! Error tolerance

      !* Loop over the desired number of steps
      time = 0
      write(*,*) 'Enter number of steps: '
      read(*,*) nStep
      do iStep=1,nStep

        !* Record values for plotting
        x = state(1)
        y = state(2)
        z = state(3)
        tplot(iStep) = time
        tauplot(iStep) = tau
        xplot(iStep) = x
        yplot(iStep) = y
        zplot(iStep) = z
        if( mod(iStep, 50) .eq. 0 ) then
          write(*,*) 'Finished ', iStep, ' steps out of ', nStep
        endif

        !* Find new state using adaptive Runge-Kutta
        call rka(state,nState,time,tau,err,lorzrk,param)
      enddo

      !* Print max and min time step returned by rka
      maxTau = tauplot(2)
      minTau = tauplot(2)
      do i=3,nStep
        if( tauplot(i) .gt. maxTau ) then
          maxTau = tauplot(i)
        else if( tauplot(i) .lt. minTau ) then
          minTau = tauplot(i)
        endif
      enddo
      write(*,*) 'Adaptive time step: Max = ', maxTau,
     &                             '  Min = ', minTau

      ! Find the location of the three steady states
      x_ss(1) = 0
      y_ss(1) = 0
      z_ss(1) = 0
      x_ss(2) = sqrt(b*(r-1))
      y_ss(2) = x_ss(2)
      z_ss(2) = r-1
      x_ss(3) = -sqrt(b*(r-1))
      y_ss(3) = x_ss(3)
      z_ss(3) = r-1

      !* Print out the plotting variables:
      !    tplot, xplot, yplot, zplot, x_ss, y_ss, z_ss
      open(11,file='tplot.txt',status='unknown')
      open(12,file='xplot.txt',status='unknown')
      open(13,file='yplot.txt',status='unknown')
      open(14,file='zplot.txt',status='unknown')
      open(15,file='x_ss.txt',status='unknown')
      open(16,file='y_ss.txt',status='unknown')
      open(17,file='z_ss.txt',status='unknown')
      do i=1,nStep
        write(11,*) tplot(i)
        write(12,*) xplot(i)
        write(13,*) yplot(i)
        write(14,*) zplot(i)
      enddo
      do i=1,3
        write(15,*) x_ss(i)
        write(16,*) y_ss(i)
        write(17,*) z_ss(i)
      enddo
      stop
      end

!***** To plot in MATLAB; use the script below ********************
!load tplot.txt; load xplot.txt; load yplot.txt; load zplot.txt;
!load x_ss.txt; load y_ss.txt; load z_ss.txt;
!%* Graph the time series x(t)
!figure(1); clf;  % Clear figure 1 window and bring forward
!plot(tplot,xplot,'-')
!xlabel('Time');  ylabel('x(t)')
!title('Lorenz model time series')
!pause(1)  % Pause 1 second
!%* Graph the x,y,z phase space trajectory
!figure(2); clf;  % Clear figure 2 window and bring forward
!plot3(xplot,yplot,zplot,'-',x_ss,y_ss,z_ss,'*')
!view([30 20]);  % Rotate to get a better view
!grid;           % Add a grid to aid perspective
!xlabel('x'); ylabel('y'); zlabel('z');
!title('Lorenz model phase space');
!*****************************************************************

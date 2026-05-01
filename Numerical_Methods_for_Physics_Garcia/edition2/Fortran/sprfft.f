! sprfft - Program to compute the power spectrum of a
! coupled mass-spring system.

      program sprfft
      integer*4 MAXnStep
      parameter( MAXnStep = 100000 )
      integer*4 nState, i, iStep, nStep, nPrint
      real*8 x(3), v(3), state(6), param(1)
      real*8 time, tau, pi, k_over_m, window
      real*8 xplot(MAXnStep,3), tplot(MAXnStep)
      real*8 f(MAXnStep), spect(MAXnStep), spectW(MAXnStep)
      complex*16 x1fft(MAXnStep), x1fftW(MAXnStep)
      external sprrk
      !* Set parameters for the system (initial positions, etc.).
      nState = 6
      write(*,*) 'Enter initial displacements x(1),x(2),x(3) = '
      read(*,*) x(1), x(2), x(3)
      v(1) = 0.0
      v(2) = 0.0    ! Masses are initially at rest
      v(3) = 0.0
      state(1) = x(1)
      state(2) = x(2)
      state(3) = x(3)
      state(4) = v(1)
      state(5) = v(2)
      state(6) = v(3)
      write(*,*) 'Enter timestep: '
      read(*,*) tau
      k_over_m = 1      ! Ratio of spring const. over mass
      param(1) = k_over_m

      !* Loop over the desired number of time steps.
      time = 0          ! Set initial time
      nStep = 256       ! Number of steps in the main loop
      nPrint = nStep/8  ! Number of steps between printing progress
      do iStep=1,nStep

        !* Use Runge-Kutta to find new displacements of the masses.
        call rk4(state,nState,time,tau,sprrk,param)
        time = time + tau

        !* Record the positions for graphing and to compute spectra.
        xplot(iStep,1) = state(1)   ! Record positions
        xplot(iStep,2) = state(2)
        xplot(iStep,3) = state(3)
        tplot(iStep) = time
        if( mod(iStep,nprint) .lt. 1 ) then
          write(*,*) 'Finished ', iStep, ' out of ', nStep, ' steps'
        endif
      enddo

      !* Calculate the power spectrum of the time series for mass #1
      do i=1,nStep
        f(i) = (i-1)/(tau*nStep)      ! Frequency
        x1fft(i) = xplot(i,1)         ! Displacement of mass 1
      enddo
      call fft(x1fft, nStep)       ! Fourier transform of displacement
      do i=1,nStep                 ! Power spectrum of displacement
        spect(i) = abs(x1fft(i))**2
      enddo

      !* Apply the Hanning window to the time series and calculate
      !  the resulting power spectrum
      pi = 3.141592654
      do i=1,nStep
        window = 0.5*(1.0-cos(2.0*pi*(i-1.0)/nStep))  ! Hanning window
        x1fftW(i) = xplot(i,1) * window               ! Windowed time series
      enddo
      call fft(x1fftW, nStep)      ! Fourier transf. (windowed data)
      do i=1,nStep                 ! Power spectrum (windowed data)
        spectW(i) = abs(x1fftW(i))**2
      enddo

      !* Print out the plotting variables:
      !    tplot, xplot, f, spect, spectw
      open(11,file='tplot.txt',status='unknown')
      open(12,file='xplot.txt',status='unknown')
      open(13,file='f.txt',status='unknown')
      open(14,file='spect.txt',status='unknown')
      open(15,file='spectw.txt',status='unknown')
      do i=1,nStep
        write(11,*) tplot(i)
        write(12,*) xplot(i,1), xplot(i,2), xplot(i,3)
        write(13,*) f(i)
        write(14,*) spect(i)
        write(15,*) spectW(i)
      enddo
      stop
      end
!***** To plot in MATLAB; use the script below ********************
!load tplot.txt; load xplot.txt; load f.txt;
!load spect.txt; load spectw.txt
!nstep = length(tplot);  nprint = nstep/8;
!%* Graph the displacements of the three masses.
!figure(1); clf;  % Clear figure 1 window and bring forward
!ipr = 1:nprint:nstep;  % Used to graph limited number of symbols
!plot(tplot(ipr),xplot(ipr,1),'o',tplot(ipr),xplot(ipr,2),'+',...
!         tplot(ipr),xplot(ipr,3),'*',...
!         tplot,xplot(:,1),'-',tplot,xplot(:,2),'-.',...
!         tplot,xplot(:,3),'--');
!legend('Mass #1','Mass #2','Mass #3');
!title('Displacement of masses (relative to rest positions)');
!xlabel('Time'); ylabel('Displacement');
!%* Graph the power spectra for original and windowed data
!figure(2); clf;  % Clear figure 2 window and bring forward
!semilogy(f(1:(nstep/2)),spect(1:(nstep/2)),'-',...
!       f(1:(nstep/2)),spectw(1:(nstep/2)),'--');
!title('Power spectrum (dashed is windowed data)');
!xlabel('Frequency'); ylabel('Power');
!******************************************************************

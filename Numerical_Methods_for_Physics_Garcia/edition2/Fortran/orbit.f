! orbit - Program to compute the orbit of a comet.

      program orbit
      integer*4 MAXnStep
      parameter( MAXnStep = 100000 )
      integer*4 nState, nStep, method, iStep, i
      real*8 r0, v0, r(2), v(2), state(4), accel(2), GM, param(1)
      real*8 pi, mass, adaptErr, time, tau, normR, normV
      real*8 rplot(MAXnStep), thplot(MAXnStep), tplot(MAXnStep)
      real*8 kinetic(MAXnStep), potential(MAXnStep)
      external gravrk  ! Function which defines equations of motion

      !* Set initial position and velocity of the comet.
      write(*,*) 'Enter initial radial distance (AU): '
      read(*,*) r0
      write(*,*) 'Enter initial tangential velocity (AU/yr): '
      read(*,*) v0
      r(1) = r0
      r(2) = 0
      v(1) = 0
      v(2) = v0
      state(1) = r(1)
      state(2) = r(2)   ! Used by R-K routines
      state(3) = v(1)
      state(4) = v(2)
      nState = 4        ! Number of elements in state vector

      !* Set physical parameters (mass, G*M)
      pi = 3.141592654
      GM = 4*pi**2      ! Grav. const. * Mass of Sun (au^3/yr^2)
      param(1) = GM
      mass = 1.         ! Mass of comet
      adaptErr = 1.e-3  ! Error parameter used by adaptive Runge-Kutta
      time = 0

      !* Loop over desired number of steps using specified
      !  numerical method.
      write(*,*) 'Enter number of steps: '
      read(*,*) nStep
      write(*,*) 'Enter time step (yr): '
      read(*,*) tau
      write(*,*) 'Choose a numerical method:'
      write(*,*) '1) Euler,       2) Euler-Cromer, '
      write(*,*) '3) Runge-Kutta, 4) Adaptive R-K: '
      read(*,*) method
      do iStep=1,nStep

        !* Record position and energy for plotting.
        normR = sqrt( r(1)*r(1) + r(2)*r(2) )
        normV = sqrt( v(1)*v(1) + v(2)*v(2) )
        rplot(iStep) = normR           ! Record position for plotting
        thplot(iStep) = atan2(r(2),r(1))
        tplot(iStep) = time
        kinetic(iStep) = 0.5*mass*normV**2   ! Record energies
        potential(iStep) = -GM*mass/normR

        !* Calculate new position and velocity using desired method.
        if( method .eq. 1 ) then
          accel(1) = -GM*r(1)/(normR**3)
          accel(2) = -GM*r(2)/(normR**3)
          r(1) = r(1) + tau*v(1)             ! Euler step
          r(2) = r(2) + tau*v(2)
          v(1) = v(1) + tau*accel(1)
          v(2) = v(2) + tau*accel(2)
          time = time + tau
        else if( method .eq. 2 ) then
          accel(1) = -GM*r(1)/(normR**3)
          accel(2) = -GM*r(2)/(normR**3)
          v(1) = v(1) + tau*accel(1)
          v(2) = v(2) + tau*accel(2)
          r(1) = r(1) + tau*v(1)             ! Euler-Cromer step
          r(2) = r(2) + tau*v(2)
          time = time + tau
        else if( method .eq. 3 ) then
          call rk4( state, nState, time, tau, gravrk, param )
          r(1) = state(1)
          r(2) = state(2)   ! 4th order Runge-Kutta
          v(1) = state(3)
          v(2) = state(4)
          time = time + tau
        else
          call rka( state, nState, time, tau, adaptErr, gravrk, param )
          r(1) = state(1)
          r(2) = state(2)   ! Adaptive Runge-Kutta
          v(1) = state(3)
          v(2) = state(4)
        endif

      enddo

      !* Print out the plotting variables:
      !    thplot, rplot, potential, kinetic
      open(11,file='thplot.txt',status='unknown')
      open(12,file='rplot.txt',status='unknown')
      open(13,file='tplot.txt',status='unknown')
      open(14,file='potential.txt',status='unknown')
      open(15,file='kinetic.txt',status='unknown')
      do i=1,nStep
        write(11,*) thplot(i)
        write(12,*) rplot(i)
        write(13,*) tplot(i)
        write(14,*) potential(i)
        write(15,*) kinetic(i)
      enddo
      stop
      end

!***** To plot in MATLAB; use the script below ********************
!load thplot.txt; load rplot.txt; load tplot.txt;
!load potential.txt; load kinetic.txt;
!%* Graph the trajectory of the comet.
!figure(1); clf;  % Clear figure 1 window and bring forward
!polar(thplot,rplot,'+');  % Use polar plot for graphing orbit
!xlabel('Distance (AU)');  grid;
!pause(1)   % Pause for 1 second before drawing next plot
!%* Graph the energy of the comet versus time.
!figure(2); clf;   % Clear figure 2 window and bring forward
!totalE = kinetic + potential;   % Total energy
!plot(tplot,kinetic,'-.',tplot,potential,'--',tplot,totalE,'-')
!legend('Kinetic','Potential','Total');
!xlabel('Time (yr)'); ylabel('Energy (M AU^2/yr^2)');
!*****************************************************************

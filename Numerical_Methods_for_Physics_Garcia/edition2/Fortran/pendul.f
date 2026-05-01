! pendul - Program to compute the motion of a simple pendulum
! using the Euler or Verlet method

      program pendul
      integer*4 MAXnStep
      parameter( MAXnStep = 100000 )
      integer*4 method, irev, iStep, nStep, nPeriod, i
      real*8 theta0, pi, theta, omega, g_over_L, time, time_old
      real*8 tau, accel, theta_old, theta_new, AvePeriod, ErrorBar
      real*8 t_plot(MAXnStep), th_plot(MAXnStep), period(MAXnStep)

      !* Select the numerical method to use: Euler or Verlet
      write(*,*) 'Choose a numerical method 1) Euler, 2) Verlet: '
      read(*,*) method

      !* Set initial position and velocity of pendulum
      write(*,*) 'Enter initial angle (in degrees): '
      read(*,*) theta0
      pi = 3.141592654
      theta = theta0*pi/180   ! Convert angle to radians
      omega = 0.0             ! Set the initial velocity

      !* Set the physical constants and other variables
      g_over_L = 1.0          ! The constant g/L
      time = 0.0              ! Initial time
      irev = 0                ! Used to count number of reversals
      write(*,*) 'Enter time step: '
      read(*,*) tau

      !* Take one backward step to start Verlet
      accel = -g_over_L*sin(theta)    ! Gravitational acceleration
      theta_old = theta - omega*tau + 0.5*tau**2*accel

      !* Loop over desired number of steps with given time step
      !    and numerical method
      write(*,*) 'Enter number of time steps: '
      read(*,*) nStep
      do iStep=1, nStep

        !* Record angle and time for plotting
        t_plot(iStep) = time
        th_plot(iStep) = theta*180/pi   ! Convert angle to degrees
        time = time + tau

        !* Compute new position and velocity using
        !    Euler or Verlet method
        accel = -g_over_L*sin(theta)    ! Gravitational acceleration
        if( method .eq. 1 ) then
          theta_old = theta        ! Save previous angle
          theta = theta + tau*omega       ! Euler method
          omega = omega + tau*accel
        else
          theta_new = 2*theta - theta_old + accel*tau**2
          theta_old = theta        ! Verlet method
          theta = theta_new
        endif

        !* Test if the pendulum has passed through theta = 0;
        !    if yes, use time to estimate period
        if( theta*theta_old .lt. 0 ) then   ! Test position for sign change
          write(*,*) 'Turning point at time t = ', time
          if( irev .eq. 0 ) then     ! If this is the first change,
            time_old = time          ! just record the time
          else
            period(irev) = 2*(time - time_old)
            time_old = time
          endif
          irev = irev+1       ! Increment the number of reversals
        endif

      enddo
      nPeriod = irev-1        ! Number of times period is measured

      !* Estimate period of oscillation, including error bar
      AvePeriod = 0.0
      ErrorBar = 0.0
      do i=1,nPeriod
        AvePeriod = AvePeriod + period(i)
      enddo
      AvePeriod = AvePeriod/nPeriod
      do i=1,nPeriod
        ErrorBar = ErrorBar + (period(i) - AvePeriod)**2
      enddo
      ErrorBar = sqrt(ErrorBar/(nPeriod*(nPeriod-1)))
      write(*,*) 'Average period = ', AvePeriod, ' +/- ', ErrorBar

      !* Print out the plotting variables: t_plot, th_plot
      open(11,file='t_plot.txt',status='unknown')
      open(12,file='th_plot.txt',status='unknown')
      do i=1,nStep
        write(11,*) t_plot(i)
        write(12,*) th_plot(i)
      enddo
      stop
      end

!***** To plot in MATLAB; use the script below ********************
!load t_plot.txt; load th_plot.txt;
!clf;  figure(gcf);         % Clear and forward figure window
!plot(t_plot,th_plot,'+');
!xlabel('Time');  ylabel('Theta (degrees)');
!******************************************************************

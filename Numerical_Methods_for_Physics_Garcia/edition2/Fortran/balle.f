! balle - Program to compute the trajectory of a baseball
!         using the Euler method.

      program balle
      integer*4 MAXmaxStep
      parameter( MAXmaxStep = 100000 )
      integer*4 iStep, maxStep, i
      real*8 y1, speed, theta, pi, airFlag, rho, Cd, area, grav, mass
      real*8 air_const, tau, t, normV
      real*8 r1(2), v1(2), r(2), v(2), accel(2)
      real*8 xplot(MAXmaxStep), yplot(MAXmaxStep)
      real*8 xNoAir(MAXmaxStep), yNoAir(MAXmaxStep)

      !* Set initial position and velocity of the baseball
      write(*,*) 'Enter initial height (meters): '
      read(*,*) y1
      r1(1) = 0      ! Initial vector position
      r1(2) = y1
      write(*,*) 'Enter initial speed (m/s): '
      read(*,*) speed
      write(*,*) 'Enter initial angle (degrees): '
      read(*,*) theta
      pi = 3.141592654
      v1(1) = speed*cos(theta*pi/180)   ! Initial velocity (x)
      v1(2) = speed*sin(theta*pi/180)   ! Initial velocity (y)
      r(1) = r1(1)
      r(2) = r1(2)      ! Set initial position and velocity
      v(1) = v1(1)
      v(2) = v1(2)

      !* Set physical parameters (mass, Cd, etc.)
      Cd = 0.35      ! Drag coefficient (dimensionless)
      area = 4.3e-3  ! Cross-sectional area of projectile (m^2)
      grav = 9.81    ! Gravitational acceleration (m/s^2)
      mass = 0.145   ! Mass of projectile (kg)

      write(*,*) 'Air resistance? (Yes:1, No:0): '
      read(*,*) airFlag
      if( airFlag .eq. 0 ) then
        rho = 0      ! No air resistance
      else
        rho = 1.2    ! Density of air (kg/m^3)
      endif
      air_const = -0.5*Cd*rho*area/mass  ! Air resistance constant

      !* Loop until ball hits ground or max steps completed
      write(*,*) 'Enter timestep, tau (sec): '
      read(*,*) tau
      maxStep = 1000   ! Maximum number of steps
      do iStep=1,maxStep

        !* Record position (computed and theoretical) for plotting
        xplot(iStep) = r(1)   ! Record trajectory for plot
        yplot(iStep) = r(2)
        t = (iStep-1)*tau        ! Current time
        xNoAir(iStep) = r1(1) + v1(1)*t
        yNoAir(iStep) = r1(2) + v1(2)*t - 0.5*grav*t**2

        !* Calculate the acceleration of the ball
        normV = sqrt( v(1)*v(1) + v(2)*v(2) )
        accel(1) = air_const*normV*v(1)    ! Air resistance
        accel(2) = air_const*normV*v(2)    ! Air resistance
        accel(2) = accel(2) - grav         ! Gravity

        !* Calculate the new position and velocity using Euler method
        r(1) = r(1) + tau*v(1)             ! Euler step
        r(2) = r(2) + tau*v(2)
        v(1) = v(1) + tau*accel(1)
        v(2) = v(2) + tau*accel(2)

        !* If ball reaches ground (y<0), break out of the loop
        if( r(2) .lt. 0 ) then
          xplot(iStep+1) = r(1)  ! Record last values computed
          yplot(iStep+1) = r(2)
          goto 100               ! Break out of the for loop
        endif
      enddo
100   continue

      !* Print maximum range and time of flight
      write(*,*) 'Maximum range is ', r(1), ' meters'
      write(*,*) 'Time of flight is ', iStep*tau, ' seconds'

      !* Print out the plotting variables:
      !    xplot, yplot, xNoAir, yNoAir
      open(11,file='xplot.txt',status='unknown')
      open(12,file='yplot.txt',status='unknown')
      open(13,file='xNoAir.txt',status='unknown')
      open(14,file='yNoAir.txt',status='unknown')
      do i=1,iStep+1
        write(11,*) xplot(i)
        write(12,*) yplot(i)
      enddo
      do i=1,iStep
        write(13,*) xNoAir(i)
        write(14,*) yNoAir(i)
      enddo
      stop
      end

!***** To plot in MATLAB; use the script below ********************
!load xplot.txt; load yplot.txt; load xNoAir.txt; load yNoAir.txt;
!clf;  figure(gcf);   % Clear figure window and bring it forward
!% Mark the location of the ground by a straight line
!xground = [0 max(xNoAir)];  yground = [0 0];
!% Plot the computed trajectory and parabolic, no-air curve
!plot(xplot,yplot,'+',xNoAir,yNoAir,'-',xground,yground,'-');
!legend('Euler method','Theory (No air)');
!xlabel('Range (m)');  ylabel('Height (m)');
!title('Projectile motion');
!*****************************************************************

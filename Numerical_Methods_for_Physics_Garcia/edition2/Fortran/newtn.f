! newtn - Program to solve a system of nonlinear equations
! using Newton's method.  Equations defined by function fnewt.

      program newtn
      integer*4 MAXnVars, MAXnParams, MAXnStep
      parameter( MAXnVars = 20, MAXnParams = 20, MAXnStep = 100 )
      integer*4 iStep, nStep, nVars, nParams, i, j
      real*8 temp
      real*8 x(MAXnVars), xp(MAXnVars,MAXnStep+1), a(MAXnParams)
      real*8 f(MAXnVars), D(MAXnVars,MAXnVars), dx(MAXnVars)
      real*8 ge, determ

      !* Set initial guess and parameters
      nStep = 10   ! Number of iterations before stopping
      nVars = 3    ! Number of variables
      nParams = 3  ! Number of parameters
      write(*,*) 'Enter the initial guess: '
      do i=1,nVars
        write(*,*) ' x(', i, ') = '
        read(*,*) x(i)
        xp(i,1) = x(i)  ! Record initial guess for plotting
      enddo
      write(*,*) 'Enter the parameters: '
      do i=1,nParams
        write(*,*) 'a(', i, ') = '
        read(*,*) a(i)
      enddo

      !* Loop over desired number of steps
      do iStep=1,nStep

        !* Evaluate function f and its Jacobian matrix D
        call fnewt(x,a,MAXnVars,f,D)  ! fnewt returns value of f and D
        do i=1,nVars
          do j=i+1,nVars
            temp = D(i,j)
            D(i,j) = D(j,i)      ! Transpose of matrix D
            D(j,i) = temp
          enddo
        enddo

        !* Find dx by Gaussian elimination
        determ = ge(D,f,nVars,MAXnVars,dx)   ! Determinant returned but not used

        !* Update the estimate for the root
        do i=1,nVars
          x(i) = x(i) - dx(i)   ! Newton iteration for new x
          xp(i,iStep+1) = x(i)  ! Save current estimate for plotting
        enddo
      enddo

      !* Print the final estimate for the root
      write(*,*) 'After ', nStep, ' iterations the root is:'
      do i=1,nVars
        write(*,*) 'x(', i, ') = ', x(i)
      enddo

      !* Print out the plotting variable: xp
      open(11,file='xp.txt',status='unknown')
      do i=1,nVars
        do j=1,nStep
          write(11,1001) xp(i,j)
        enddo
        write(11,*) xp(i,nStep+1)
      enddo
1001  format(e12.6,', ',$)   ! The $ suppresses the carriage return
      stop
      end

!***** To plot in MATLAB; use the script below ********************
!load xp.txt;
!%* Plot the iterations from initial guess to final estimate
!figure(1); clf;  % Clear figure 1 window and bring forward
!subplot(1,2,1) % Left plot
!      plot3(xp(1,:),xp(2,:),xp(3,:),'o-');
!      xlabel('x');  ylabel('y'); zlabel('z');
!      view([-37.5, 30]);  % Viewing angle
!      grid; drawnow;
!subplot(1,2,2) % Right plot
!      plot3(xp(1,:),xp(2,:),xp(3,:),'o-');
!      xlabel('x');  ylabel('y'); zlabel('z');
!      view([-127.5, 30]);  % Viewing angle
!      grid; drawnow;
!% Plot data from lorenz (if available).
!flag = input('Plot data from lorenz program? (1=Yes/0=No): ');
!if( flag == 1 )
!      figure(2); clf;  % Clear figure 1 window and bring forward
!      load xplot.txt; load yplot.txt; load zplot.txt;
!      plot3(xplot,yplot,zplot,'-',xp(1,:),xp(2,:),xp(3,:),'o--');
!      xlabel('x'); ylabel('y'); zlabel('z');
!      view([40 10]);  % Rotate to get a better view
!      grid;           % Add a grid to aid perspective
!end
!******************************************************************

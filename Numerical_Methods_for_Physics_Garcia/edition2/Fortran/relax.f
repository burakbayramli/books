! relax - Program to solve the Laplace equation using
! Jacobi, Gauss-Seidel and SOR methods on a square grid

      program relax
      integer*4 MAXN, MAXiterMax
      parameter( MAXN = 200, MAXiterMax = MAXN*MAXN )
      integer*4 method, N, i, j, iterMax, iter, nIter
      real*8 L, h, x(MAXN), y(MAXN), omega, omegaOpt, pi, phi0
      real*8 coeff, phi(MAXN,MAXN), newphi(MAXN,MAXN), phiTemp
      real*8 changeDesired, change(MAXiterMax), changeSum

      !* Initialize parameters (system size, grid spacing, etc.)
      write(*,*) 'Select a numerical method:'
      write(*,*) '   1) Jacobi, 2) Gauss-Seidel, 3) SOR : '
      read(*,*) method
      write(*,*) 'Enter number of grid points on a side: '
      read(*,*) N
      L = 1             ! System size (length)
      h = L/(N-1)       ! Grid spacing
      do i=1,N
        x(i) = (i-1)*h   ! x coordinate
        y(i) = x(i)      ! y coordinate
      enddo

      !* Select over-relaxation factor (SOR only)
      pi = 3.141592654
      if( method .eq. 3 ) then
        omegaOpt = 2.0/(1.0+sin(pi/N))   ! Theoretical optimum
        write(*,*) 'Theoretical optimum omega = ', omegaOpt
        write(*,*) 'Enter desired omega: '
        read(*,*) omega
      endif

      !* Set initial guess as first term in separation of variables soln.
      phi0 = 1       ! Potential at y=L
      coeff = phi0 * 4/(pi*sinh(pi))
      do i=1,N
        do j=1,N
          phi(i,j) = coeff * sin(pi*x(i)/L) * sinh(pi*y(j)/L)
        enddo
      enddo

      !* Set boundary conditions
      do i=1,N
        phi(i,1) = 0.0
        phi(i,N) = phi0
      enddo
      do j=1,N
        phi(1,j) = 0.0
        phi(N,j) = 0.0
      enddo
      write(*,*) 'Potential at y=L equals ', phi0
      write(*,*) 'Potential is zero on all other boundaries'

      !* Loop until desired fractional change per iteration is obtained
      do i=1,N
       do j=1,N
         newphi(i,j) = phi(i,j) ! Copy of the solution (used only by Jacobi)
       enddo
      enddo
      iterMax = N*N             ! Set max to avoid excessively long runs
      changeDesired = 1e-4      ! Stop when the change is given fraction
      write(*,*) 'Desired fractional change = ', changeDesired
      do iter=1,iterMax

        changeSum = 0
        if( method .eq. 1 ) then  !! Jacobi method !!
          do i=2,(N-1)            ! Loop over interior points only
           do j=2,(N-1)
             newphi(i,j) = 0.25*(phi(i+1,j)+phi(i-1,j)+
     &                           phi(i,j-1)+phi(i,j+1))
             changeSum = changeSum + abs(1-phi(i,j)/newphi(i,j))
           enddo
          enddo
          do i=2,(N-1)            ! Loop over interior points only
           do j=2,(N-1)
             phi(i,j) = newphi(i,j)   ! Copy new values into phi
           enddo
          enddo
        else if( method .eq. 2 ) then  !! G-S method !!
          do i=2,(N-1)            ! Loop over interior points only
           do j=2,(N-1)
             phiTemp = 0.25*(phi(i+1,j)+phi(i-1,j)+
     &                       phi(i,j-1)+phi(i,j+1))
             changeSum = changeSum + abs(1-phi(i,j)/phiTemp)
             phi(i,j) = phiTemp
           enddo
          enddo
        else                    !! SOR method !!
          do i=2,(N-1)            ! Loop over interior points only
           do j=2,(N-1)
             phiTemp = 0.25*omega*(phi(i+1,j)+phi(i-1,j)+
     &                  phi(i,j-1)+phi(i,j+1))  +  (1-omega)*phi(i,j)
             changeSum = changeSum + abs(1-phi(i,j)/phiTemp)
             phi(i,j) = phiTemp
           enddo
          enddo
        endif

        !* Check if fractional change is small enough to halt the iteration
        change(iter) = changeSum/(N-2)**2
        if( mod(iter,10) .lt. 1 ) then
          write(*,*) "After ", iter,
     &               " iterations, fractional change = ", change(iter)
        endif
        if( change(iter) .lt. changeDesired ) then
          write(*,*) "Desired accuracy achieved after ", iter,
     &                                                 " iterations"
          write(*,*) "Breaking out of main loop"
          nIter = iter
          goto 1        ! Break out of the main loop
        endif
      enddo
1     continue

      !* Print out the plotting variables: x, y, phi, change
      open(11,file='x.txt',status='unknown')
      open(12,file='y.txt',status='unknown')
      open(13,file='phi.txt',status='unknown')
      open(14,file='change.txt',status='unknown')
      do i=1,N
        write(11,*) x(i)
        write(12,*) y(i)
        do j=1,(N-1)
          write(13,1001) phi(i,j)
        enddo
        write(13,*) phi(i,N)
      enddo
      do i=1,nIter
        write(14,*) change(i)
      enddo
1001  format(e12.6,', ',$)   ! The $ suppresses the carriage return

      stop
      end
!***** To plot in MATLAB; use the script below ********************
!load x.txt; load y.txt; load phi.txt; load change.txt;
!%* Plot final estimate of potential as contour and surface plots
!figure(1); clf;
!cLevels = 0:(0.1):1;    % Contour levels
!cs = contour(x,y,flipud(rot90(phi)),cLevels);
!xlabel('x'); ylabel('y'); clabel(cs);
!figure(2); clf;
!mesh(x,y,flipud(rot90(phi)));
!xlabel('x'); ylabel('y'); zlabel('\Phi(x,y)');
!%* Plot the fractional change versus iteration
!figure(3); clf;
!semilogy(change);
!xlabel('Iteration');  ylabel('Fractional change');
!******************************************************************

!  schro - Program to solve the Schrodinger equation
!  for a free particle using the Crank-Nicolson scheme

      program schro
      integer*4 MAXN, MAXnplots
      parameter( MAXN = 200, MAXnplots = 100 )
      integer*4 N, i, j, k, nStep, nplots, iplot, iStep
      real*8 L, h, h_bar, mass, tau, x(MAXN), eye(MAXN,MAXN)
      real*8 ham(MAXN,MAXN), coeff, pi, x0, velocity, k0, sigma0
      real*8 Norm_psi, expFactor, plotStep, p_plot(MAXN, MAXnplots+2)
      complex*16 iImag, A(MAXN,MAXN), B(MAXN,MAXN), Ai(MAXN,MAXN)
      complex*16 D(MAXN,MAXN), Psi(MAXN), PsiInit(MAXN)
      complex*16 NewPsi(MAXN)

      !* Initialize parameters (grid spacing, time step, etc.)
      write(*,*) 'Enter number of grid points: '
      read(*,*) N
      L = 100        ! System extends from -L/2 to L/2
      h = L/(N-1)    ! Grid size
      h_bar = 1      ! Planck's constant (natural units)
      mass = 1       ! Particle mass (natural units)
      write(*,*) 'Enter time step: '
      read(*,*) tau
      do i=1,N
        x(i) = h*(i-1) - L/2   ! Coordinates  of grid points
      enddo

      !* Set up the Hamiltonian operator matrix
      do i=1,N
       do j=1,N
         eye(i,j) = 0.0  ! Set all elements to zero
         ham(i,j) = 0.0  ! then evaluate non-zero elements
       enddo
      enddo
      do i=1,N
        eye(i,i) = 1.0   ! Identity matrix
      enddo
      coeff = -h_bar**2/(2*mass*h**2)
      do i=2,(N-1)
        ham(i,i-1) = coeff
        ham(i,i) = -2*coeff   ! Set interior rows
        ham(i,i+1) = coeff
      enddo
      ! First and last rows for periodic boundary conditions
      ham(1,N) = coeff
      ham(1,1) = -2*coeff
      ham(1,2) = coeff
      ham(N,N-1) = coeff
      ham(N,N) = -2*coeff
      ham(N,1) = coeff

      !* Compute the Crank-Nicolson matrix
      iImag = ( 0.0, 1.0 )    ! = sqrt(-1)
      do i=1,N
       do j=1,N
         A(i,j) = eye(i,j) + iImag*0.5*tau/h_bar*ham(i,j)
         B(i,j) = eye(i,j) - iImag*0.5*tau/h_bar*ham(i,j)
       enddo
      enddo
      write(*,*) 'Computing matrix inverse ... '
      call cinv( A, N, MAXN, Ai )         ! Complex matrix inverse
      write(*,*) ' ... done'
      do i=1,N
       do j=1,N
        D(i,j) = 0.0           ! Matrix (complex) multiplication
        do k=1,N
          D(i,j) = D(i,j) + Ai(i,k)*B(k,j) ! Crank-Nicolson matrix
        enddo
       enddo
      enddo

      !* Initialize the wavefunction
      pi = 3.141592654
      x0 = 0           ! Location of the center of the wavepacket
      velocity = 0.5   ! Average velocity of the packet
      k0 = mass*velocity/h_bar        ! Average wavenumber
      sigma0 = L/10    ! Standard deviation of the wavefunction
      Norm_psi = 1/(sqrt(sigma0*sqrt(pi)))   ! Normalization
      do i=1,N
        expFactor = exp(-(x(i)-x0)**2/(2*sigma0**2))
        Psi(i) = Norm_psi * cos(k0*x(i)) * expFactor
     &  + iImag* Norm_psi * sin(k0*x(i)) * expFactor
        PsiInit(i) = Psi(i)    ! Record initial wavefunction
      enddo

      !* Initialize loop and plot variables
      nStep = int(L/(velocity*tau))  ! Particle should circle system
      nplots = 20                    ! Number of plots to record
      plotStep = nStep/nplots        ! Iterations between plots
      do i=1,N                       ! Record initial condition
        p_plot(i,1) = abs(Psi(i))**2
      enddo
      iplot = 1

      !* Loop over desired number of steps (wave circles system once)
      do iStep=1,nStep

        !* Compute new wave function using the Crank-Nicolson scheme
        do i=1,N        ! Matrix multiply D*psi
          NewPsi(i) = 0
          do j=1,N
            NewPsi(i) = NewPsi(i) + D(i,j)*Psi(j)
          enddo
        enddo
        do i=1,N
          Psi(i) = NewPsi(i)  ! Copy new values into Psi
        enddo

        !* Periodically record values for plotting
        if( (iStep-int(iStep/plotStep)*plotStep) .lt. 1 ) then
          iplot = iplot + 1
          do i=1,N
            p_plot(i,iplot) = abs(Psi(i))**2
          enddo
          write(*,*) 'Finished ', iStep, ' of ', nStep, ' steps'
        endif
      enddo
      ! Record final probability density
      iplot = iplot + 1
      do i=1,N
        p_plot(i,iplot) = abs(Psi(i))**2
      enddo
      nplots = iplot    ! Actual number of plots recorded

      !* Print out the plotting variables:
      !     x, real(PsiInit), imag(PsiInit), p_plot
      open(11,file='x.txt',status='unknown')
      open(12,file='rpi.txt',status='unknown')
      open(13,file='ipi.txt',status='unknown')
      open(14,file='p_plot.txt',status='unknown')
      do i=1,N
        write(11,*) x(i)
        write(12,*) real(PsiInit(i))
        write(13,*) imag(PsiInit(i))
        do j=1,(nplots-1)
          write(14,1001) p_plot(i,j)
        enddo
        write(14,*) p_plot(i,nplots)
      enddo
1001  format(e12.6,', ',$)   ! The $ suppresses the carriage return

      stop
      end
!***** To plot in MATLAB; use the script below ********************
!load x.txt; load rpi.txt; load ipi.txt; load p_plot.txt;
!%* Plot the initial wavefunction
!figure(1); clf;
!plot(x,rpi,x,ipi);
!title('Initial wave function');
!xlabel('x');  ylabel('\psi(x)'); legend('Real','Imag');
!%* Plot probability versus position at various times
!figure(2); clf;
![mp np] = size(p_plot);
!plot(x,p_plot(:,1:3:np),x,p_plot(:,np));
!xlabel('x'); ylabel('P(x,t)');
!title('Probability density at various times');
!axisV = [-1/2 1/2 0 max(p_plot)]; % Fix axis min and max
!*****************************************************************

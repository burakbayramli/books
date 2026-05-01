! fftpoi - Program to solve the Poisson equation using
! MFT method (periodic boundary conditions)

      program fftpoi
      integer*4 MAXN
      parameter( MAXN = 300 )
      integer*4 N, i, j, M, ii, jj
      real*8 eps0, L, h, x(MAXN), y(MAXN), rho(MAXN,MAXN)
      real*8 xc, yc, q, pi, cx(MAXN), cy(MAXN), numerator
      real*8 tinyNumber, phi(MAXN,MAXN)
      complex*16 P(MAXN,MAXN), R(MAXN,MAXN), F(MAXN,MAXN)

      !* Initialize parameters (system size, grid spacing, etc.)
      eps0 = 8.8542e-12   ! Permittivity (C^2/(N m^2))
      N = 64   ! Number of grid points on a side (square grid)
      L = 1    ! System size
      h = L/N  ! Grid spacing for periodic boundary conditions
      do i=1,N
        x(i) = (i-0.5)*h   ! Coordinates of grid points
        y(i) = x(i)        ! on a square grid
      enddo
      write(*,*) 'System is a square of length ', L

      !* Set up charge density rho(i,j)
      do i=1,N
       do j=1,N
         rho(i,j) = 0.0  ! Initialize charge density to zero
       enddo
      enddo
      write(*,*) 'Enter number of line charges: '
      read(*,*) M
      do i=1,M
        write(*,*) 'For charge #', i
        write(*,*) 'Enter x,y coordinates: '
        read(*,*) xc,yc
        ii = int(xc/h) + 1    ! Place charge at nearest
        jj = int(yc/h) + 1    ! grid point
        write(*,*) 'Enter charge density: '
        read(*,*) q
        rho(ii,jj) = rho(ii,jj) + q/h**2
      enddo

      !* Compute matrix P
      pi = 3.141592654
      do i=1,N
        cx(i) = cos((2*pi/N)*(i-1))
        cy(i) = cx(i)
      enddo
      numerator = -h**2/(2*eps0)
      tinyNumber = 1e-20  ! Avoids division by zero
      do i=1,N
       do j=1,N
         P(i,j) = numerator/(cx(i)+cy(j)-2.0+tinyNumber)
       enddo
      enddo

      !* Compute potential using MFT method
      do i=1,N
       do j=1,N
         R(i,j) = rho(i,j)    ! Copy rho into R for input to fft2
       enddo
      enddo
      call fft2(R,N,MAXN)   ! Transform rho into wavenumber domain
      ! Compute phi in the wavenumber domain
      do i=1,N
       do j=1,N
         F(i,j) = R(i,j)*P(i,j)
       enddo
      enddo
      call ifft2(F,N,MAXN)    ! Inv. transf. phi into the coord. domain
      do i=1,N
       do j=1,N
         phi(i,j) = real(F(i,j))
       enddo
      enddo

      !* Print out the plotting variables: x, y, phi
      open(11,file='x.txt',status='unknown')
      open(12,file='y.txt',status='unknown')
      open(13,file='phi.txt',status='unknown')
      do i=1,N
        write(11,*) x(i)
        write(12,*) y(i)
        do j=1,(N-1)
          write(13,1001) phi(i,j)
        enddo
        write(13,*) phi(i,N)
      enddo
1001  format(e12.6,', ',$)   ! The $ suppresses the carriage return

      stop
      end
!***** To plot in MATLAB; use the script below ********************
!load x.txt; load y.txt; load phi.txt;
!%* Compute electric field as E = - grad phi
![Ex Ey] = gradient(flipud(rot90(phi)));
!magnitude = sqrt(Ex.^2 + Ey.^2);
!Ex = -Ex ./ magnitude;     % Normalize components so
!Ey = -Ey ./ magnitude;     % vectors have equal length
!%* Plot potential and electric field
!figure(1); clf;
!contour3(x,y,flipud(rot90(phi,1)),35);
!xlabel('x'); ylabel('y'); zlabel('\Phi(x,y)');
!figure(2); clf;
!quiver(x,y,Ex,Ey)        % Plot E field with vectors
!title('E field (Direction)'); xlabel('x'); ylabel('y');
!axis('square');  axis([0 1 0 1]);
!*****************************************************************

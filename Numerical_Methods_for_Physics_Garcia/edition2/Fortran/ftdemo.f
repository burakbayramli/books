! ftdemo - Discrete Fourier transform demonstration program
      program ftdemo
      integer*4 MAXN
      parameter( MAXN = 16384 )
      integer*4 N, i, j, k, method
      real*8 twoPiN, freq, phase, tau, pi, t(MAXN), y(MAXN)
      real*8 f(MAXN), powSpec(MAXN)
      complex*16 ii, yt(MAXN)

  !* Initialize the sine wave time series to be transformed.
      write(*,*) 'Enter the number of points: '
      read(*,*) N
      write(*,*) 'Enter frequency of the sine wave: '
      read(*,*) freq
      write(*,*) 'Enter phase of the sine wave: '
      read(*,*) phase
      tau = 1   ! Time increment
      pi = 3.141592654
      do i=1,N
        t(i) = (i-1)*tau                     ! t = [0, tau, 2*tau, ... ]
        y(i) = sin(2*pi*t(i)*freq + phase)   ! Sine wave time series
        f(i) = (i-1)/(N*tau)                 ! f = [0, 1/(N*tau), ... ]
      enddo

      !* Compute the transform using desired method: direct summation
      !  or fast Fourier transform (FFT) algorithm.
      write(*,*) 'Compute transform by, 1) Direct summation; 2) FFT: '
      read(*,*) method
      if( method .eq. 1 ) then            ! Direct summation
        twoPiN = -2*pi/N
        ii = (0., 1.)            ! ii = sqrt(-1)
        do k=0,N
          yt(k+1) = (0.0, 0.0)
          do j=0,(N-1)
            yt(k+1) = yt(k+1) +
     &                y(j+1)*cos(twoPiN*j*k) +
     &                ii*y(j+1)*sin(twoPiN*j*k)
          enddo
        enddo
      else                          ! Fast Fourier transform
        do i=1,N
          yt(i) = y(i)        ! Copy data for input to fft
        enddo
        call fft(yt,N)
      endif
      !* Compute the power spectrum
      do k=1,N
        powSpec(k) = abs(yt(k))**2
      enddo

      !* Print out the plotting variables:
      !    t, y, f, ytReal, ytImag, powspec
      open(11,file='t.txt',status='unknown')
      open(12,file='y.txt',status='unknown')
      open(13,file='f.txt',status='unknown')
      open(14,file='ytReal.txt',status='unknown')
      open(15,file='ytImag.txt',status='unknown')
      open(16,file='powSpec.txt',status='unknown')
      do i=1,N
        write(11,*) t(i)
        write(12,*) y(i)
        write(13,*) f(i)
        write(14,*) real(yt(i))
        write(15,*) imag(yt(i))
        write(16,*) powSpec(i)
      enddo
      stop
      end

!***** To plot in MATLAB; use the script below ********************
!load t.txt; load y.txt; load f.txt;
!load ytReal.txt; load ytImag.txt; load powSpec.txt;
!%* Graph the time series and its transform.
!figure(1); clf;  % Clear figure 1 window and bring forward
!plot(t,y);
!title('Original time series');
!ylabel('Amplitude');  xlabel('Time');
!figure(2); clf;  % Clear figure 2 window and bring forward
!plot(f,ytReal,'-',f,ytImag,'--');
!legend('Real','Imaginary');
!title('Fourier transform');
!ylabel('Transform');  xlabel('Frequency');
!%* Compute and graph the power spectrum of the time series.
!figure(3); clf;  % Clear figure 3 window and bring forward
!semilogy(f,powSpec,'-');
!title('Power spectrum (unnormalized)');
!ylabel('Power');  xlabel('Frequency');
!******************************************************************

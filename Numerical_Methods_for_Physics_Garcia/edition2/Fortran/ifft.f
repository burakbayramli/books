      subroutine ifft( A, N )
      integer*4 N
      complex*16 A(*)
! Routine to compute inverse Fourier transform using FFT algorithm
! Inputs
!    A         Complex transform data vector
!    N         Number of data points
! Outputs
!    A         Complex time series

      integer*4 i

      !* Take complex conjugate of input transform
      do i=1,N
        A(i) = conjg(A(i))     ! Complex conjugate
      enddo

      !* Evaluate fast fourier transform
      call fft( A, N )

      !* Take complex conjugate and normalize by N
      do i=1,N
        A(i) = conjg(A(i))/N  ! Normalize and complex conjugate
      enddo

      return
      end

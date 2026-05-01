      subroutine fft( A, N )
      integer*4 N
      complex*16 A(N)
! Routine to compute discrete Fourier transform using FFT algorithm
! Inputs
!    A         Complex data vector
! Outputs
!    A         Complex transform of input data vector

      integer*4 M, NN, N_half, Nm1, i, j, k, ke, ke1, ip
      real*8 pi, angle, temp
      complex*16 U, W, T

      !* Determine size of input data and check that it is power of 2
      M = int( alog(float(N))/alog(2.0) + 0.5)  ! N = 2^M
      NN = int( 2.0**M + 0.5 )
      if( N .ne. NN ) then
        write(*,*) "ERROR in fft(): Number of points not power of 2"
        return
      endif
      pi =  4.0d0*datan(1.0d0)
      N_half = N/2
      Nm1 = N-1

      !* Bit-scramble the input data by swapping elements
      j=1
      do i=1,Nm1
        if( i .lt. j ) then
          T = A(j)     ! Swap elements i and j
          A(j) = A(i)  ! of RealA and ImagA
          A(i) = T
        endif
        k = N_half
1       if( k .ge. j ) goto 2  ! While loop
          j = j-k
          k = k/2
          goto 1
2       continue
        j = j+k
      enddo

      !* Loop over number of layers, M = log_2(N)
      do k=1,M
        ke = 2**k
        ke1 = ke/2
        !* Compute lowest, non-zero power of W for this layer
        U = (1.0, 0.0)
        angle = -pi/ke1
        W = cmplx(cos(angle), sin(angle))
        !* Loop over elements in binary order (outer loop)
        do j=1,ke1
          !* Loop over elements in binary order (inner loop)
          do i=j,N,ke
            ip = i + ke1
            !* Compute the y(.)*W^. factor for this element
            T = A(ip)*U
            !* Update the current element and its binary pair
            A(ip) = A(i)-T
            A(i)  = A(i)+T
          enddo
          !* Increment the power of W for next set of elements
          U = U*W
        enddo
      enddo
      return
      end

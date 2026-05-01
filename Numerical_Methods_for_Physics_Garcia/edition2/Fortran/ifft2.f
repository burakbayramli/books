      subroutine ifft2( A, N, MAXN )
      integer*4 N, MAXN
      complex*16 A(MAXN,MAXN)
! Routine to compute inverse two dimensional Fourier transform
! using FFT algorithm
! Inputs
!    A         Complex transform array
!    N         Elements transformed are A(1,1) to A(N,N)
!   MAXN       Array dimensioned as A(MAXN,MAXN)
! Outputs
!    A         Complex data array

      integer*4 MAXNT
      parameter( MAXNT = 2048 )
      integer*4 i, j
      real*8 invN2
      complex*16 T(MAXNT)  ! Temporary work vector

      !* Loop over the columns of the matrix
      do j=1,N
        !* Copy out a column into a vector and take its complex conjugate
        do i=1,N
          T(i) = conjg(A(i,j))
        enddo
        !* Take FFT of the vector
        call fft( T, N )
        !* Copy the transformed vector back into the column
        do i=1,N
          A(i,j) = T(i)
        enddo
      enddo

      !* Loop over the rows of the matrix
      invN2 = 1.0/N**2
      do i=1,N
        !* Copy out a row into a vector and take its complex conjugate
        do j=1,N
          T(j) = A(i,j)
        enddo
        !* Take FFT of the vector
        call fft( T, N )
        !* Copy the transformed vector back, taking its complex conjugate
        !  and applying the 1/N normalization
        do j=1,N
          A(i,j) = conjg(T(j))*invN2
        enddo
      enddo

      return
      end

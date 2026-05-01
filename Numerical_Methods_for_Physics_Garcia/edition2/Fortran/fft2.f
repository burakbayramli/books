      subroutine fft2( A, N, MAXN )
      integer*4 N, MAXN
      complex*16 A(MAXN,MAXN)
! Routine to compute two dimensional Fourier transform
! using FFT algorithm
! Inputs
!    A         Complex input data array
!    N         Elements transformed are A(1,1) to A(N,N)
!   MAXN       Array dimensioned as A(MAXN,MAXN)
! Outputs
!    A         Complex transform of data

      integer*4 MAXNT
      parameter( MAXNT = 2048 )
      integer*4 i, j
      complex*16 T(MAXNT)  ! Temporary work vector

      !* Loop over the columns of the matrix
      do j=1,N
        !* Copy out a column into a vector
        do i=1,N
          T(i) = A(i,j)
        enddo
        !* Take FFT of the vector
        call fft(T,N)
        !* Copy the transformed vector back into the column
        do i=1,N
          A(i,j) = T(i)
        enddo
      enddo

      !* Loop over the rows of the matrix
      do i=1,N
        !* Copy out a row into a vector
        do j=1,N
          T(j) = A(i,j)
        enddo
        !* Take FFT of the vector
        call fft(T,N)
        !* Copy the transformed vector back into the row
        do j=1,N
          A(i,j) = T(j)
        enddo
      enddo
      return
      end

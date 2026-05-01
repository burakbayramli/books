      real*8 function trige( A, b, N, x)
      integer*4 N
      real*8 A(*,3), b(*), x(*)
! Function to solve b = A*x by Gaussian elimination where
! the matrix A is a packed tridiagonal matrix
! Inputs
!   A      Packed tridiagonal matrix, N by N unpacked
!   b      Column vector
!   N      Number of elements used in matrix A and vector b
! Output
!   x      Solution of b = A*x
! determ   Determinant of A

      parameter( MAXN = 500 )
      integer*4 i
      real*8 alpha(MAXN), beta(MAXN), gamma(MAXN), coeff, determ

      !* Unpack diagonals of triangular matrix into vectors
      do i=1,(N-1)
        alpha(i) = A(i+1,1)
        beta(i) = A(i,2)
        gamma(i) = A(i,)
      enddo
      beta(N) = A(N,2)

      !* Perform forward elimination
      do i=2,N
        coeff = alpha(i-1)/beta(i-1)
        beta(i) = beta(i) - coeff*gamma(i-1)
        b(i) = b(i) - coeff*b(i-1)
      enddo

      !* Compute determinant as product of diagonal elements
      determ = 1.0
      do i=1,N
        determ = determ*beta(i)
      enddo

      !* Perform back substitution
      x(N) = b(N)/beta(N)
      do i=(N-1),1,-1
        x(i) = (b(i) - gamma(i)*x(i+1))/beta(i)
      enddo

      trige = determ ! Return the determinant
      return
      end

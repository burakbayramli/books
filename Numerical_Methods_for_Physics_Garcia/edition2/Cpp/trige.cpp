#include "NumMeth.h"

double trige( Matrix A, Matrix b, Matrix& x) {
// Function to solve b = A*x by Gaussian elimination where
// the matrix A is a packed tridiagonal matrix
// Inputs
//   A      Packed tridiagonal matrix, N by N unpacked
//   b      Column vector of length N
// Output 
//   x      Solution of b = A*x; Column vector of length N
// determ   Determinant of A

  //* Check that dimensions of a and b are compatible
  int N = A.nRow();
  assert( N == b.nRow() && A.nCol() == 3 );

  //* Unpack diagonals of triangular matrix into vectors
  Matrix alpha(N), beta(N), gamma(N);
  int i;
  for( i=1; i<=(N-1); i++ ) {
    alpha(i) = A(i+1,1);
    beta(i) = A(i,2);
    gamma(i) = A(i,3);
  }
  beta(N) = A(N,2);

  //* Perform forward elimination
  for( i=2; i<=N; i++ )	{
    double coeff = alpha(i-1)/beta(i-1);
    beta(i) -= coeff*gamma(i-1);
    b(i) -= coeff*b(i-1);
  }

  //* Compute determinant as product of diagonal elements
  double determ = 1.0;
  for( i=1; i<=N; i++ )
	determ *= beta(i);

  //* Perform back substitution
  x(N) = b(N)/beta(N);
  for( i=N-1; i>=1; i--)
    x(i) = (b(i) - gamma(i)*x(i+1))/beta(i);

  return( determ );
}

#include "NumMeth.h"

// Compute inverse of matrix
double inv(Matrix A, Matrix& Ainv) 
// Input
//    A    -    Matrix A (N by N)
// Outputs
//   Ainv  -    Inverse of matrix A (N by N)
//  determ -    Determinant of matrix A	(return value)
{

  int N = A.nRow();
  assert( N == A.nCol() );
  
  Ainv = A;  // Copy matrix to ensure Ainv is same size
    
  int i, j, k;
  Matrix scale(N), b(N,N);	 // Scale factor and work array
  int *index;  index = new int [N+1];

  //* Matrix b is initialized to the identity matrix
  b.set(0.0);
  for( i=1; i<=N; i++ )
    b(i,i) = 1.0;

  //* Set scale factor, scale(i) = max( |a(i,j)| ), for each row
  for( i=1; i<=N; i++ ) {
    index[i] = i;			  // Initialize row index list
    double scalemax = 0.;
    for( j=1; j<=N; j++ ) 
      scalemax = (scalemax > fabs(A(i,j))) ? scalemax : fabs(A(i,j));
    scale(i) = scalemax;
  }

  //* Loop over rows k = 1, ..., (N-1)
  int signDet = 1;
  for( k=1; k<=N-1; k++ ) {
	//* Select pivot row from max( |a(j,k)/s(j)| )
    double ratiomax = 0.0;
	int jPivot = k;
    for( i=k; i<=N; i++ ) {
      double ratio = fabs(A(index[i],k))/scale(index[i]);
      if( ratio > ratiomax ) {
        jPivot=i;
        ratiomax = ratio;
      }
    }
	//* Perform pivoting using row index list
	int indexJ = index[k];
	if( jPivot != k ) {	          // Pivot
      indexJ = index[jPivot];
      index[jPivot] = index[k];   // Swap index jPivot and k
      index[k] = indexJ;
	  signDet *= -1;			  // Flip sign of determinant
	}
	//* Perform forward elimination
    for( i=k+1; i<=N; i++ ) {
      double coeff = A(index[i],k)/A(indexJ,k);
      for( j=k+1; j<=N; j++ )
        A(index[i],j) -= coeff*A(indexJ,j);
      A(index[i],k) = coeff;
      for( j=1; j<=N; j++ ) 
        b(index[i],j) -= A(index[i],k)*b(indexJ,j);
    }
  }
  //* Compute determinant as product of diagonal elements
  double determ = signDet;	   // Sign of determinant
  for( i=1; i<=N; i++ )
	determ *= A(index[i],i);

  //* Perform backsubstitution
  for( k=1; k<=N; k++ ) {
    Ainv(N,k) = b(index[N],k)/A(index[N],N);
    for( i=N-1; i>=1; i--) {
      double sum = b(index[i],k);
      for( j=i+1; j<=N; j++ )
        sum -= A(index[i],j)*Ainv(j,k);
      Ainv(i,k) = sum/A(index[i],i);
    }
  }

  delete [] index;	// Release allocated memory
  return( determ );        
}

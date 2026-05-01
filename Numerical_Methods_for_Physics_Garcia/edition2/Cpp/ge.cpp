#include "NumMeth.h"

// ge - Function to perform Gaussian elimination to solve A*x = b
//      using scaled column pivoting
// Inputs
//    A    -    Matrix A (N by N)
//    b    -    Vector b (N by 1)
// Outputs
//    x    -    Vector x (N by 1)
//  determ -    Determinant of matrix A	 (return value)
double ge(Matrix A, Matrix b, Matrix& x) {

  int N = A.nRow();	 
  assert( N == A.nCol() && N == b.nRow() && N == x.nRow() );
    
  int i, j, k;
  Matrix scale(N);	// Scale factor
  int *index;  index = new int [N+1];	// Row index list
  
  //* Set scale factor, scale(i) = max( |A(i,j)| ), for each row
  for( i=1; i<=N; i++ ) {
    index[i] = i;		   // Initialize row index list
    double scaleMax = 0.0;
    for( j=1; j<=N; j++ ) 
      scaleMax = (scaleMax > fabs(A(i,j))) ? scaleMax : fabs(A(i,j));
    scale(i) = scaleMax;
  }

  //* Loop over rows k = 1, ..., (N-1)
  int signDet = 1;
  for( k=1; k<=(N-1); k++ ) {
	//* Select pivot row from max( |A(j,k)/s(j)| )
    double ratiomax = 0.0;
	int jPivot = k;
    for( i=k; i<=N; i++ ) {
      double ratio = fabs(A(index[i],k))/scale(index[i]);
      if( ratio > ratiomax ) {
        jPivot = i;
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
      b(index[i]) -= A(index[i],k)*b(indexJ);
    }
  }
  //* Compute determinant as product of diagonal elements
  double determ = signDet;	   // Sign of determinant
  for( i=1; i<=N; i++ )
	determ *= A(index[i],i);

  //* Perform backsubstitution
  x(N) = b(index[N])/A(index[N],N);
  for( i=N-1; i>=1; i-- ) {
    double sum = b(index[i]);
    for( j=i+1; j<=N; j++ )
      sum -= A(index[i],j)*x(j);
    x(i) = sum/A(index[i],i);
  }
  
  delete [] index;	 // Release allocated memory
  return( determ );        
}

#include "NumMeth.h"

// Compute inverse of complex matrix
void cinv( Matrix RealA, Matrix ImagA, 
			 Matrix& RealAinv, Matrix& ImagAinv ) 
// Inputs
//   RealA  -    Real part of matrix A (N by N)
//   ImagA  -    Imaginary part of matrix A (N by N)
// Outputs
//   RealAinv  -    Real part of inverse of matrix A (N by N)
//   ImagAinv  -    Imaginary part of A inverse (N by N)
{

  int N = RealA.nRow();
  assert( N == RealA.nCol() && N == ImagA.nRow() 
	                        && N == ImagA.nCol());
    RealAinv = RealA; // Copy matrices to ensure they are same size
  ImagAinv = ImagA;
  
  int i, j, k;
  Matrix scale(N);	 // Scale factor
  int *index;  index = new int [N+1];

  //* Matrix B is initialized to the identity matrix
  Matrix RealB(N,N), ImagB(N,N);
  RealB.set(0.0);  ImagB.set(0.0);
  for( i=1; i<=N; i++ )
    RealB(i,i) = 1.0;

  //* Set scale factor, scale(i) = max( |a(i,j)| ), for each row
  for( i=1; i<=N; i++ ) {
    index[i] = i;			  // Initialize row index list
    double scaleMax = 0.;
    for( j=1; j<=N; j++ ) {
	  double MagA = RealA(i,j)*RealA(i,j) + ImagA(i,j)*ImagA(i,j);
      scaleMax = (scaleMax > MagA) ? scaleMax : MagA;
    }
	scale(i) = scaleMax;
  }

  //* Loop over rows k = 1, ..., (N-1)
  for( k=1; k<=N-1; k++ ) {
	//* Select pivot row from max( |a(j,k)/s(j)| )
    double ratiomax = 0.0;
	int jPivot = k;
    for( i=k; i<=N; i++ ) {
	  double MagA = RealA(index[i],k)*RealA(index[i],k) + 
		            ImagA(index[i],k)*ImagA(index[i],k);
      double ratio = MagA/scale(index[i]);
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
	}
	//* Perform forward elimination
    for( i=k+1; i<=N; i++ ) {
	  double denom = RealA(indexJ,k)*RealA(indexJ,k) 
		           + ImagA(indexJ,k)*ImagA(indexJ,k);
      double RealCoeff = (RealA(index[i],k)*RealA(indexJ,k)
		               + ImagA(index[i],k)*ImagA(indexJ,k))/denom;
      double ImagCoeff = (ImagA(index[i],k)*RealA(indexJ,k)
		               - RealA(index[i],k)*ImagA(indexJ,k))/denom;
      for( j=k+1; j<=N; j++ ) {
        RealA(index[i],j) -= RealCoeff*RealA(indexJ,j)
		                   - ImagCoeff*ImagA(indexJ,j);
        ImagA(index[i],j) -= RealCoeff*ImagA(indexJ,j)
		                   + ImagCoeff*RealA(indexJ,j);
      }
	  RealA(index[i],k) = RealCoeff;
	  ImagA(index[i],k) = ImagCoeff;
      for( j=1; j<=N; j++ ) {
        RealB(index[i],j) -= RealA(index[i],k)*RealB(indexJ,j)
			               - ImagA(index[i],k)*ImagB(indexJ,j);
        ImagB(index[i],j) -= RealA(index[i],k)*ImagB(indexJ,j)
			               + ImagA(index[i],k)*RealB(indexJ,j);
	  }
    }
  }

  //* Perform backsubstitution
  for( k=1; k<=N; k++ ) {
	double denom = RealA(index[N],N)*RealA(index[N],N) 
		         + ImagA(index[N],N)*ImagA(index[N],N);
    RealAinv(N,k) = (RealB(index[N],k)*RealA(index[N],N) 
		          + ImagB(index[N],k)*ImagA(index[N],N))/denom;
    ImagAinv(N,k) = (ImagB(index[N],k)*RealA(index[N],N) 
		          - RealB(index[N],k)*ImagA(index[N],N))/denom;
    for( i=N-1; i>=1; i--) {
      double RealSum = RealB(index[i],k);
      double ImagSum = ImagB(index[i],k);
      for( j=i+1; j<=N; j++ ) {
        RealSum -= RealA(index[i],j)*RealAinv(j,k)
			     - ImagA(index[i],j)*ImagAinv(j,k);
        ImagSum -= RealA(index[i],j)*ImagAinv(j,k)
			     + ImagA(index[i],j)*RealAinv(j,k);
	  }
	  double denom = RealA(index[i],i)*RealA(index[i],i) 
		           + ImagA(index[i],i)*ImagA(index[i],i);
      RealAinv(i,k) = (RealSum*RealA(index[i],i) 
		            + ImagSum*ImagA(index[i],i))/denom;
      ImagAinv(i,k) = (ImagSum*RealA(index[i],i) 
		            - RealSum*ImagA(index[i],i))/denom;
    }
  }

  delete [] index;	// Release allocated memory
}

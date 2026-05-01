#include "NumMeth.h"

void fft( Matrix& RealA, Matrix& ImagA);

void fft2( Matrix& RealA, Matrix& ImagA) {
// Routine to compute two dimensional Fourier transform 
// using FFT algorithm
// Inputs
//    RealA, ImagA         Real and imaginary parts of data array
// Outputs
//    RealA, ImagA         Real and imaginary parts of transform

  int i, j, N = RealA.nRow();
  Matrix RealT(N), ImagT(N);  // Temporary work vector

  //* Loop over the columns of the matrix
  for( j=1; j<=N; j++ ) {
	//* Copy out a column into a vector
    for( i=1; i<=N; i++ ) {
      RealT(i) = RealA(i,j);  
      ImagT(i) = ImagA(i,j);
    }
	//* Take FFT of the vector
    fft(RealT,ImagT);         
	//* Copy the transformed vector back into the column
    for( i=1; i<=N; i++ ) {
      RealA(i,j) = RealT(i); 
      ImagA(i,j) = ImagT(i);
    }
  }

  //* Loop over the rows of the matrix
  for( i=1; i<=N; i++ ) {
	//* Copy out a row into a vector
    for( j=1; j<=N; j++ ) {
      RealT(j) = RealA(i,j);  
      ImagT(j) = ImagA(i,j);
    }
	//* Take FFT of the vector
    fft(RealT,ImagT);         
	//* Copy the transformed vector back into the row
    for( j=1; j<=N; j++ ) {
      RealA(i,j) = RealT(j);  
      ImagA(i,j) = ImagT(j);
    }
  }
}


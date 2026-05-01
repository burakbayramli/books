#include "NumMeth.h"

void fft( Matrix& RealA, Matrix& ImagA);

void ifft( Matrix& RealA, Matrix& ImagA) {
// Routine to compute inverse Fourier transform using FFT algorithm
// Inputs
//    RealA, ImagA         Real and imaginary parts of transform
// Outputs
//    RealA, ImagA         Real and imaginary parts of time series

  int i, N = RealA.nRow();   // Number of data points

  //* Take complex conjugate of input transform
  for( i=1; i<=N; i++ )
    ImagA(i) *= -1.0;        // Complex conjugate

  //* Evaluate fast fourier transform  
  fft( RealA, ImagA );       
  
  //* Take complex conjugate and normalize by N
  double invN = 1.0/N;
  for( i=1; i<=N; i++ ) {
    RealA(i) *= invN;
    ImagA(i) *= -invN;    // Normalize and complex conjugate
  }
}

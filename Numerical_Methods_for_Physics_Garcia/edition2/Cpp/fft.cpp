#include "NumMeth.h"

void fft( Matrix& RealA, Matrix& ImagA) {
// Routine to compute discrete Fourier transform using FFT algorithm
// Inputs
//    RealA, ImagA         Real and imaginary parts of data vector
// Outputs
//    RealA, ImagA         Real and imaginary parts of transform

  double RealU, RealW, RealT, ImagU, ImagW, ImagT;

  //* Determine size of input data and check that it is power of 2
  int N = RealA.nRow();  // Number of data points
  int M = (int)(log( (double)N )/log(2.0) + 0.5);  // N = 2^M
  int NN = (int)(pow(2.0,(double)M) + 0.5);
  if( N != NN ) {
    cerr << "ERROR in fft(): Number of data points not power of 2" << endl;
    return;
  }
  const double pi = 3.141592654;
  int N_half = N/2;
  int Nm1 = N-1;
  
  //* Bit-scramble the input data by swapping elements
  int i,k,j=1;
  for( i=1; i<=Nm1; i++ ) {
    if( i < j ) {
      RealT = RealA(j);     ImagT = ImagA(j);	  // Swap elements i and j 
      RealA(j) = RealA(i);  ImagA(j) = ImagA(i);  // of RealA and ImagA
      RealA(i) = RealT;     ImagA(i) = ImagT;
    }
    k = N_half;
    while( k < j ) {
      j -= k;
      k /= 2;
    }
    j += k;
  }

  //* Loop over number of layers, M = log_2(N)
  for( k=1; k<=M; k++ ) {
    int ke = (int)(pow(2.0,(double)k) + 0.5);
    int ke1 = ke/2;
	//* Compute lowest, non-zero power of W for this layer
    RealU = 1.0;  ImagU = 0.0;
    double angle = -pi/ke1;
    RealW = cos(angle);  ImagW = sin(angle);
	//* Loop over elements in binary order (outer loop)
    for( j=1; j<=ke1; j++ ) {
	  //* Loop over elements in binary order (inner loop)
      for( i=j; i<=N; i+=ke ) {
        int ip = i + ke1;
		//* Compute the y(.)*W^. factor for this element
        RealT = RealA(ip)*RealU - ImagA(ip)*ImagU;	// T = A(ip)*U
        ImagT = RealA(ip)*ImagU + ImagA(ip)*RealU;
		//* Update the current element and its binary pair
        RealA(ip) = RealA(i)-RealT;
        ImagA(ip) = ImagA(i)-ImagT;	   // A(ip) = A(i) - T
        RealA(i) += RealT;
        ImagA(i) += ImagT;			   // A(i) = A(i) + T
      }
	  //* Increment the power of W for next set of elements
      double temp = RealU*RealW - ImagU*ImagW;
      ImagU = RealU*ImagW + ImagU*RealW;	   // U = U * W
      RealU = temp;
    }
  }
}

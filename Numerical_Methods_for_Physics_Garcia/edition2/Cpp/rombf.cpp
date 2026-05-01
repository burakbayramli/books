#include "NumMeth.h"

void rombf( double a, double b, int N, 
		   double (*func)( double x, Matrix param ),
           Matrix param, Matrix& R) {
//  Function to compute integrals by Romberg algorithm
//  R = rombf(a,b,N,func,param)
//  Inputs
//    a,b    Lower and upper bound of the integral
//    N      Romberg table is N by N
//    func   Integrand function; the calling sequence 
//           is: double (*func)( double x, Matrix param )
//    param  Set of parameters to be passed to function
//  Output 
//     R     Romberg table; Entry R(N,N) is best estimate of
//           the value of the integral

  //* Compute the first term R(1,1)
  double h = b - a;     // This is the coarsest panel size
  int np = 1;           // Current number of panels
  R(1,1) = h/2 * ((*func)(a,param) + (*func)(b,param));

  //* Loop over the desired number of rows, i = 2,...,N
  int i,j,k;
  for( i=2; i<=N; i++ ) {

    //* Compute the summation in the recursive trapezoidal rule
    h /= 2.0;          // Use panels half the previous size
    np *= 2;           // Use twice as many panels
    double sumT = 0.0;
    for( k=1; k<=(np-1); k+=2 ) 
      sumT += (*func)( a + k*h, param);

    //* Compute Romberg table entries R(i,1), R(i,2), ..., R(i,i)
    R(i,1) = 0.5 * R(i-1,1) + h * sumT;   
    int m = 1;
    for( j=2; j<=i; j++ ) {
      m *= 4;
      R(i,j) = R(i,j-1) + (R(i,j-1) - R(i-1,j-1))/(m-1);
    }
  }
}


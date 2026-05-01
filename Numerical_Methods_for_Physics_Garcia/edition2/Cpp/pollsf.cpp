#include "NumMeth.h"

void inv(Matrix a, Matrix& aInv);

void pollsf( Matrix x, Matrix y, Matrix sigma, int M, 
             Matrix& a_fit, Matrix& sig_a, Matrix& yy, double& chisqr) {
// Function to fit a polynomial to data
// Inputs 
//   x       Independent variable
//   y       Dependent variable
//   sigma   Estimate error in y
//   M       Number of parameters used to fit data
// Outputs
//   a_fit   Fit parameters; a(1) is intercept, a(2) is slope
//   sig_a   Estimated error in the parameters a()
//   yy      Curve fit to the data
//   chisqr  Chi squared statistic

  //* Form the vector b and design matrix A
  int i, j, k, N = x.nRow();
  Matrix b(N), A(N,M);
  for( i=1; i<=N; i++ ) {
   b(i) = y(i)/sigma(i);
   for( j=1; j<=M; j++ )
     A(i,j) = pow(x(i),(double)(j-1))/sigma(i);  
  }


  //* Compute the correlation matrix C 
  Matrix C(M,M), Cinv(M,M);
  for( i=1; i<=M; i++ ) {   // (C inverse) = (A transpose) * A
    for( j=1; j<=M; j++ ) {   
      Cinv(i,j) = 0.0;
      for( k=1; k<=N; k++ )
        Cinv(i,j) += A(k,i)*A(k,j);
    }
  }
  inv( Cinv, C );  // C = ( (C inverse) inverse)
  
  //* Compute the least squares polynomial coefficients a_fit
  for( k=1; k<=M; k++ ) {
    a_fit(k) = 0.0;
    for( j=1; j<=M; j++ )
     for( i=1; i<=N; i++ )
       a_fit(k) += C(k,j) * A(i,j) * b(i);
  }

  //* Compute the estimated error bars for the coefficients
  for( j=1; j<=M; j++ )
    sig_a(j) = sqrt(C(j,j));                          

  //* Evaluate curve fit at each data point and compute Chi^2
  chisqr = 0.0;
  for( i=1; i<=N; i++ ) {
    yy(i) = 0.0;      // yy is the curve fit
    for( j=1; j<=M; j++ )
      yy(i) += a_fit(j) * pow( x(i), (double)(j-1) );  
    double delta = (y(i)-yy(i))/sigma(i);
    chisqr += delta*delta;  // Chi square
  }
}

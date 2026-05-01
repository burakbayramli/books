#include "NumMeth.h"

void linreg(Matrix x, Matrix y, Matrix sigma,
            Matrix &a_fit, Matrix &sig_a, Matrix &yy, double &chisqr) {
// Function to perform linear regression (fit a line)
// Inputs
//   x       Independent variable
//   y       Dependent variable
//   sigma   Estimated error in y
// Outputs
//   a_fit   Fit parameters; a(1) is intercept, a(2) is slope
//   sig_a   Estimated error in the parameters a()
//   yy      Curve fit to the data
//   chisqr  Chi squared statistic

  //* Evaluate various sigma sums
  int i, nData = x.nRow();
  double sigmaTerm;
  double s = 0.0, sx = 0.0, sy = 0.0, sxy = 0.0, sxx = 0.0;
  for( i=1; i<=nData; i++ ) {
    sigmaTerm = 1.0/(sigma(i)*sigma(i));  
    s += sigmaTerm;              
    sx += x(i) * sigmaTerm;
    sy += y(i) * sigmaTerm;
    sxy += x(i) * y(i) * sigmaTerm;
    sxx += x(i) * x(i) * sigmaTerm;
  }
  double denom = s*sxx - sx*sx;

  //* Compute intercept a_fit(1) and slope a_fit(2)
  a_fit(1) = (sxx*sy - sx*sxy)/denom;
  a_fit(2) = (s*sxy - sx*sy)/denom;

  //* Compute error bars for intercept and slope
  sig_a(1) = sqrt(sxx/denom);
  sig_a(2) = sqrt(s/denom);

  //* Evaluate curve fit at each data point and compute Chi^2
  chisqr = 0.0;
  for( i=1; i<=nData; i++ ) {
    yy(i) = a_fit(1)+a_fit(2)*x(i);     // Curve fit to the data
    double delta = (y(i)-yy(i))/sigma(i);
    chisqr += delta*delta;  // Chi square
  }
}

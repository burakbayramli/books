#include "NumMeth.h"

void lorzrk(double X[], double t, double param[], double deriv[]) {
//  Returns right-hand side of Lorenz model ODEs
//  Inputs
//    X      State vector [x y z]
//    t      Time (not used)
//    param  Parameters [r sigma b]
//  Output
//    deriv  Derivatives [dx/dt dy/dt dz/dt]

  //* For clarity, unravel input vectors
  double x = X[1]; double y = X[2]; double z = X[3];
  double r = param[1]; double sigma = param[2]; double b = param[3];

  //* Return the derivatives [dx/dt dy/dt dz/dt]
  deriv[1] = sigma*(y-x);
  deriv[2] = r*x - y - x*z;
  deriv[3] = x*y - b*z;
  return;
}
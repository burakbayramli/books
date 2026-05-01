#include "NumMeth.h"

double errintg( double x, Matrix param) {
// Error function integrand
// Inputs
//    x       Value where integrand is evaluated
//    param   Parameter list (not used)
// Output
//    f       Integrand of the error function
  double f = exp(-x*x);
  return( f );
}

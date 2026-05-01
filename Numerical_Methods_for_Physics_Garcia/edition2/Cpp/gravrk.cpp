#include "NumMeth.h"

void gravrk(double x[], double t, double param[], double deriv[]) {
//  Returns right-hand side of Kepler ODE; used by Runge-Kutta routines
//  Inputs
//    x      State vector [r(1) r(2) v(1) v(2)]
//    t      Time (not used)
//    param     Parameter G*M (gravitational const. * solar mass)
//  Output
//    deriv  Derivatives [dr(1)/dt dr(2)/dt dv(1)/dt dv(2)/dt]

  //* Compute acceleration
  double GM = param[1];
  double r1 = x[1], r2 = x[2];     // Unravel the vector s into 
  double v1 = x[3], v2 = x[4];     // position and velocity
  double normR = sqrt( r1*r1 + r2*r2 );
  double accel1 = -GM*r1/(normR*normR*normR);  // Gravitational acceleration
  double accel2 = -GM*r2/(normR*normR*normR);  

  //* Return derivatives [dr[1]/dt dr[2]/dt dv[1]/dt dv[2]/dt]
  deriv[1] = v1;       deriv[2] = v2;
  deriv[3] = accel1;   deriv[4] = accel2;
}

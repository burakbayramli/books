// 3D adaptive quadrature over 1st octant of a torus
#include <stdio.h>
#include <math.h>
#include "integral.h"

//------------------------------------------------------------------- globals
double eps,  r, R, xvar, yvar;

//----------------------------------------------------------------- integrand
double Func(double x, double y, double z)
{
   double dR, dr, Rp, rp;
   Rp = sqrt(x*x + y*y);                             // major radial distance
   dR = R - Rp;
   rp = sqrt(dR*dR + z*z);                           // minor radial distance
   dr = 1e0 - rp/r;
   return (rp <= r ? dr*dr : 0e0);                            // zero-padding
}

//-------------------------------------------------------- integration limits
double ay(double x)
   { return (x <= R-r ? sqrt((R-r)*(R-r) - x*x) : 0.0); }
double by(double x)
   { return sqrt((R+r)*(R+r) - x*x); }
double az(double x, double y)
   { return 0.0; }
double bz(double x, double y)
   { double r0 = R - sqrt(x*x + y*y); return sqrt(fabs(r*r - r0*r0)); }

//---------------------------------------------------------------- interfaces
double Fz(double z)                                            // z-integrand
{
   return Func(xvar,yvar,z);                   // (x,y) as global (xvar,yvar)
}
double Fy(double y)                                            // y-integrand
{
   yvar = y;                                              // stores y in yvar
   return qRomberg(Fz,az(xvar,y),bz(xvar,y),eps);         // x as global xvar
}
double Fx(double x)                                            // x-integrand
{
   xvar = x;                                              // stores x in xvar
   return qRomberg(Fy,ay(x),by(x),eps);
}

//---------------------------------------------------------------------------
int main()
{
   double ax, bx, I;

   R = 3e0; r = 1e0;
   ax = 0e0; bx = R + r; eps = 1e-6;
   I = 8e0 * qRomberg(Fx,ax,bx,eps);                        // outer integral
   printf("Integral = %f\n",I);                         // result 9.869605...
}

// 3D adaptive quadrature over 1st octant of unity sphere
#include <stdio.h>
#include <math.h>
#include "integral.h"

//------------------------------------------------------------------- globals
double eps, xvar, yvar;

//----------------------------------------------------------------- integrand
double Func(double x, double y, double z) { return 1e0; }

//-------------------------------------------------------- integration limits
double ay(double x) { return 0e0; }
double by(double x) { return sqrt(1e0 - x*x); }
double az(double x, double y) { return 0.0; }
double bz(double x, double y) { return sqrt(fabs(1e0 - x*x - y*y)); }

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

   ax = 0e0; bx = 1e0; eps = 1e-7;

   I = qRomberg(Fx,ax,bx,eps);                              // outer integral
   printf("Integral = %f\n",I);                        // result 0.5235987...
}

// Plot cylindrical Bessel functions
#include <math.h>
#include "memalloc.h"
#include "specfunc.h"
#include "graphlib.h"

#define pi 3.141592653589793

double CBessJ(int n, double x)  // cylindrical Bessel function of order n+1/2
{
    return sqrt(2e0*x/pi) * SBessj(n,x);
}
double CBessN(int n, double x) // cylindrical Neumann function of order n+1/2
{
    return sqrt(2e0*x/pi) * SBessy(n,x);
}

int main(int argc, wchar_t** argv)
{
   int i, n;
   double h, xmin, xmax;
   double *x, *y;
   
   PyGraph w(argc, argv);
   w.GraphInit(1200,600);

   xmin = 0e0; xmax = 50e0; h = 0.1e0;
   n = int((xmax-xmin)/h + 0.5) + 1;

   x = Vector(1,n);
   y = Vector(1,n);

   for (i=1; i<=n; i++) {
      x[i] = xmin + (i-1)*h;
      y[i] = CBessJ(5,x[i]);
   }

   w.Plot(x,y,n,"blue",1,0.10,0.45,0.15,0.85,
          "x","J_11/2","Cylindrical Bessel function");

   xmin = 5e0; xmax = 50e0; h = 0.1e0;
   n = int((xmax-xmin)/h + 0.5) + 1;

   for (i=1; i<=n; i++) {
      x[i] = xmin + (i-1)*h;
      y[i] = CBessN(5,x[i]);
   }

   w. Plot(x,y,n,"red",1,0.60,0.95,0.15,0.85,
           "x","N_11/2","Cylindrical Neumann function");

   w.MainLoop();
}

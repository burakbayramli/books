// Plot polynomial and derivative
#include "memalloc.h"
#include "elemfunc.h"
#include "graphlib.h"

int main(int argc, wchar_t** argv)
{
   const int np = 5;                                  // degree of polynomial
   double a[] = {63e0/8e0, 0e0, -70e0/8e0, 0e0, 15e0/8e0, 0e0};     // coeffs
   double b[np+1];                                              // work array
   double h, xmin, xmax, *x, *y, *z;
   int i, n;

   xmin = -1e0; xmax = 1e0;                                // plotting domain
   h = 0.01e0;                                            // argument spacing
   n = int((xmax-xmin)/h + 0.5) + 1;                      // number of points

   x = Vector(1,n); y = Vector(1,n); z = Vector(1,n);     // arrays for plots

   PolyDerive(a,b,np);                     // coefficients of derivative in b

   for (i=1; i<=n; i++) {
      x[i] = xmin + (i-1)*h;                                      // argument
      y[i] = Poly(x[i],a,np);                                   // polynomial
      z[i] = Poly(x[i],b,np-1);                                 // derivative
   }

   PyGraph w(argc, argv);
   w.GraphInit(1200,600);

   w.Plot(x,y,n,"blue",1,0.10,0.45,0.15,0.85,"x","P(x)","Polynomial");
   w.Plot(x,z,n,"red" ,1,0.60,0.95,0.15,0.85,"x","P'(x)","Derivative");

   w.MainLoop();
}

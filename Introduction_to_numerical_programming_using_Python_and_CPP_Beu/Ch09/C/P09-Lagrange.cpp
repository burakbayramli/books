// Lagrange interpolation for a sequence of arguments
#include <stdio.h>
#include "memalloc.h"
#include "modfunc.h"
#include "graphlib.h"

int main(int argc, wchar_t** argv)
{
   double *x, *xi, *xp, *y, *yi, *yp;
   double h;
   int i, n, ni, n1, n2;
   int nn[4], sty[4];                   // ending indexes and styles of plots
   const char* col[4];                                     // colors of plots

   n  = 8;                                           // number of data points
   ni = 100;                                // number of interpolation points
   n1 = n + ni; n2 = n + 2*ni;                                 // end indexes

   x  = Vector(1,n ); y  = Vector(1,n );                       // data points
   xi = Vector(1,ni); yi = Vector(1,ni);              // interpolation points
   xp = Vector(1,n2); yp = Vector(1,n2);                   // plotting arrays

   x[1] = 0.15; x[2] = 0.2; x[3] = 0.3; x[4] = 0.5;           // data points:
   x[5] = 0.8 ; x[6] = 1.1; x[7] = 1.4; x[8] = 1.7;           // f(x) = 1/x
   for (i=1; i<=n; i++) y[i] = 1e0/x[i];

   h = (x[n]-x[1])/(ni-1);
   for (i=1; i<=ni; i++) {
      xi[i] = x[1] + (i-1)*h;                      // interpolation arguments
      yi[i] = Lagrange(x,y,n,xi[i]);                    // interpolant values
   }

   PyGraph w(argc, argv);
   w.GraphInit(800,600);

   for (i=1; i<=n; i++) {                          // fill in plotting arrays
      xp[i] = x[i]; yp[i] = y[i];                              // data points
   }
   for (i=1; i<=ni; i++) {
      xp[n +i] = xi[i]; yp[n +i] = yi[i];                      // interpolant
      xp[n1+i] = xi[i]; yp[n1+i] = 1e0/xi[i];            // original function
   }

   nn[1] = n ; col[1] = "red"  ; sty[1] =  0;                  // data points
   nn[2] = n1; col[2] = "blue" ; sty[2] =  1;                  // interpolant
   nn[3] = n2; col[3] = "black"; sty[3] = -1;            // original function
   w.MultiPlot(xp,yp,yp,nn,col,sty,3,10,0e0,0e0,0,0e0,0e0,0,
               0.15,0.95,0.15,0.85,"x","P(x)","Lagrange interpolation");

   w.MainLoop();
}

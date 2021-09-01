// Lagrange interpolation and Neville's algorithm
#include <stdio.h>
#include "memalloc.h"
#include "modfunc.h"
#include "graphlib.h"

int main(int argc, wchar_t** argv)
{
   double *err, *x, *xp, *y, *yp;
   double h, xi;
   int i, n, ni, n1, n2;
   int nn[4], sty[4];                   // ending indexes and styles of plots
   const char* col[4];                                     // colors of plots

   n  = 8;                                           // number of data points
   ni = 50;                                 // number of interpolation points
   n1 = n + ni; n2 = n + 2*ni;                                 // end indexes

   x  = Vector(1,n ); y  = Vector(1,n );                       // data points
   xp = Vector(1,n2); yp = Vector(1,n2);                   // plotting arrays
   err = Vector(1,n2);                                // interpolation errors

   x[1] = 0.15; x[2] = 0.2; x[3] = 0.3; x[4] = 0.5;       // uneven abscissas
   x[5] = 0.8 ; x[6] = 1.1; x[7] = 1.4; x[8] = 1.7;
// h = (x[n]-x[1])/(n-1);
// for (i=1; i<=n; i++) x[i] = x[1] + (i-1)*h;    // equally spaced abscissas
   for (i=1; i<=n; i++) { 
      xp[i] = x[i];
      yp[i] = y[i] = 1e0/x[i];
   }

   PyGraph w(argc, argv);
   w.GraphInit(1200,600);

//---------------------------------------------------- Lagrange interpolation
   h = (x[n]-x[1])/(ni-1);
   for (i=1; i<=ni; i++) {                         // fill in plotting arrays
      xi = x[1] + (i-1)*h;                          // interpolation argument
      xp[n +i] = xi; yp[n +i] = Lagrange(x,y,n,xi);            // interpolant
      xp[n1+i] = xi; yp[n1+i] = 1e0/xi;                  // original function
   }
   nn[1] = n ; col[1] = "red"  ; sty[1] =  0;                  // data points
   nn[2] = n1; col[2] = "blue" ; sty[2] =  1;                  // interpolant
   nn[3] = n2; col[3] = "black"; sty[3] = -1;            // original function
   w.MultiPlot(xp,yp,err,nn,col,sty,3,10,0e0,0e0,0,0e0,0e0,0,
               0.07,0.47,0.15,0.85,"x","P(x)","Lagrange interpolation");

//----------------------------------------------------- Neville interpolation
   h = (x[n]-x[1])/(ni-1);
   for (i=1; i<=ni; i++) {                         // fill in plotting arrays
      xi = x[1] + (i-1)*h;                          // interpolation argument
      xp[n +i] = xi; yp[n +i] = Neville(x,y,n,xi,err[n+i]);    // interpolant
      xp[n1+i] = xi; yp[n1+i] = 1e0/xi;                  // original function
   }
   nn[1] = n ; col[1] = "red"  ; sty[1] =  0;                  // data points
   nn[2] = n1; col[2] = "blue" ; sty[2] = -4;                  // interpolant
   nn[3] = n2; col[3] = "black"; sty[3] = -1;            // original function
   w.MultiPlot(xp,yp,err,nn,col,sty,3,10,0e0,0e0,0,0e0,0e0,0,
               0.57,0.97,0.15,0.85,"x","P(x)","Neville interpolation");

   w.MainLoop();
}

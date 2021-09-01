// Intensity distribution for Fraunhofer diffraction
#include <math.h>
#include "memalloc.h"
#include "roots.h"
#include "graphlib.h"

#define pi 3.141592653589793

double I0;                                               // maximum intensity

double Intens(double x)                                          // intensity
{
   double sinc;
   sinc = (x ? sin(x)/x : 1e0);
   return I0 * sinc * sinc;
}
double dIntens(double x)                              // derivative of Intens
{
   double sinc;
   if (x) {
      sinc = sin(x)/x;
      return 2e0 * I0 * (cos(x) - sinc) * sinc / x;
   } else { return 0e0; }
}
double func(double x)                  // function for half-width calculation
{
   return Intens(x) - 0.5e0 * I0;
}

int main(int argc, wchar_t** argv)
{
   int i, ierr, n; 
   double a, b, h, xi, xmin, xmax, *x, *y;
   int sty[3], nn[3];
   const char* col[3]; 

   PyGraph w(argc, argv);
   w.GraphInit(800,600);

   I0 = 1e0;                                             // maximum intensity
   xmin = -10e0; xmax = 10e0;

//---------------------------------------------------------------------------
   n = 101;
   x = Vector(1,2*n); y = Vector(1,2*n);

   h = (xmax-xmin) / (n-1);
   for (i=1; i<=n; i++) {
      xi = xmin + (i-1)*h;
      x[i  ] = xi; y[i  ] = Intens(xi);
      x[i+n] = xi; y[i+n] = dIntens(xi);
   }

   nn[1] = n  ; col[1] = "red" ; sty[1] =  1;
   nn[2] = 2*n; col[2] = "blue"; sty[2] = -1;                  // dashed line
   w.MultiPlot(x,y,y,nn,col,sty,2,10,
               0e0,0e0,0,0e0,0e0,0,0.15,0.85,0.15,0.85,
               "x","I, I'","Fraunhofer diffraction");

//---------------------------------------------------------------------------
   xi = pi/2e0;
   ierr = Secant(func,0e0,pi,xi);                 // half-width of major peak
   printf("Half-width of major peak = %8.5f\n",xi);

//---------------------------------------------------------------------------
   printf("\nPositions of intensity maxima:\n");
   h = 0.5e0;
   a = xmin;
   while (a < xmax) {                                 // root separation loop
      b = a + h;                                 // new search interval [a,b]
      ierr = FalsPos(dIntens,a,b,xi);
      if ((ierr == 0) && (xi != b) && (Intens(xi) > 1e-10))
         printf("%8.5f  in  (%5.2f,%5.2f)\n",xi,a,b);
      a = b;                                           // shift left boundary
   }

   w.MainLoop();
}

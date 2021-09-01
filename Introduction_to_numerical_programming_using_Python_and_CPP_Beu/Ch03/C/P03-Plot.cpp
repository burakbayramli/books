// Plot a function of one variable
#include <math.h>
#include "memalloc.h"
#include "graphlib.h"

double Func(double x)                               // function to be plotted
   { return pow(x,3) * exp(-x); }

int main(int argc, wchar_t** argv)
{
   double *x, *y;                       // declare dynamic arrays as pointers
   double h, xmin, xmax;
   int i, n;

   xmin = -0.8; xmax = 7.8;                // limits of the plotting interval
   n = 50;                                                // number of points

   x = Vector(1,n);              // allocate arrays for coordinates of points
   y = Vector(1,n);

   h = (xmax-xmin)/(n-1);                                 // argument spacing
   for (i=1; i<=n; i++) {
      x[i] = xmin + (i-1)*h;                                     // arguments
      y[i] = Func(x[i]);                                   // function values
   }

   PyGraph w(argc, argv);                       // create Tkinter root widget
   w.GraphInit(800,600);                                     // create canvas
                                                               // create plot
   w.Plot(x,y,n,"blue",1,0.15,0.95,0.15,0.85,"x","y","x^3 * exp(-x)");

   w.MainLoop();                                  // enter Tkinter event loop
}

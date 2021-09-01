// Plot a function of one variable with different styles
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

   xmin = 0.2; xmax = 9.8;                 // limits of the plotting interval
   n = 30;                                                // number of points

   x = Vector(1,n);              // allocate arrays for coordinates of points
   y = Vector(1,n);

   h = (xmax-xmin)/(n-1);                                 // argument spacing
   for (i=1; i<=n; i++) {
      x[i] = xmin + (i-1)*h;                                     // arguments
      y[i] = Func(x[i]);                                   // function values
   }

   PyGraph w(argc, argv);                       // create Tkinter root widget
   w.GraphInit(1200,800);                                    // create canvas
                                                              // create plots
   w.Plot(x,y,n,"blue",0,0.08,0.48,0.56,0.92,"None","y","Scatter plot sty = 0");
   w.Plot(x,y,n,"red" ,2,0.56,0.96,0.56,0.92,"x","y","Polar plot sty = 2");
   w.Plot(x,y,n,"red" ,3,0.08,0.48,0.08,0.44,"x","y","Drop lines sty = 3");
   w.Plot(x,y,n,"blue",4,0.56,0.96,0.08,0.44,"x","None","Histogram sty = 4");

   w.MainLoop();                                  // enter Tkinter event loop
}

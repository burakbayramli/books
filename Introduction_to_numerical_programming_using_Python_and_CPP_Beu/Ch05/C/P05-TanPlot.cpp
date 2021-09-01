// Plots the tangent function using the continued fraction representation
#include "memalloc.h"
#include "elemfunc.h"
#include "graphlib.h"

int main(int argc, wchar_t** argv)
{
   double h, xmin, xmax, *x, *y;
   int i, n;

   xmin = -1.5e0; xmax = 1.5e0;
   h = 0.01e0;
   n = int((xmax-xmin)/h) + 1;

   x = Vector(1,n); y = Vector(1,n);                      // arrays for plots

   for (i=1; i<=n; i++) {
      x[i] = xmin + (i-1)*h;
      y[i] = Tan(x[i]);
   }

   PyGraph w(argc, argv);
   w.GraphInit(600,600);

   w.Plot(x,y,n,"blue",1,0.2,0.9,0.1,0.9,"x","Tan(x)","");

   w.MainLoop();
}

// Plots two functions of one variable on the same graph
#include <math.h>
#include "memalloc.h"
#include "graphlib.h"

double Func(double x)                               // function to be plotted
   { return pow(x,3) * exp(-x); }

int main(int argc, wchar_t** argv)
{
   double *x, *y;                       // declare dynamic arrays as pointers
   double h, xmin, xmax;
   int i, n1, n2;
   const int nplot = 2;                                    // number of plots
   int n[nplot+1];                                 // ending indexes of plots
   int sty[nplot+1];                                       // styles of plots
   const char* col[nplot+1];                               // colors of plots

   xmin = -0.82; xmax = 7.8;                   // limits of plotting interval
   n1 = 30;                                     // number of points for set 1
   n2 = 50;                                     // number of points for set 2

   x = Vector(1,n1+n2);          // allocate arrays for coordinates of points
   y = Vector(1,n1+n2);

   h = (xmax-xmin)/(n1-1);                               // spacing for set 1
   for (i=1; i<=n1; i++) {
      x[i] = xmin + (i-1)*h;                                     // arguments
      y[i] = Func(x[i]);                                   // function values
   }

   h = (xmax-xmin)/(n2-1);                               // spacing for set 2
   for (i=1; i<=n2; i++) {
      x[n1+i] = xmin + (i-1)*h;                                  // arguments
      y[n1+i] = Func(x[n1+i]) * 0.9;                       // function values
   }

   PyGraph w(argc, argv);                       // create Tkinter root widget
   w.GraphInit(800,600);                                     // create canvas

   n[1] = n1   ; col[1] = "red" ; sty[1] = 0;                 // scatter plot
   n[2] = n1+n2; col[2] = "blue"; sty[2] = 1;                    // line plot
   w.MultiPlot(x,y,y,n,col,sty,nplot,10,0e0,0e0,0,0e0,0e0,0,  // create plots
               0.15,0.95,0.15,0.85,"x","y","Multiple plots");

   w.MainLoop();                                  // enter Tkinter event loop
}

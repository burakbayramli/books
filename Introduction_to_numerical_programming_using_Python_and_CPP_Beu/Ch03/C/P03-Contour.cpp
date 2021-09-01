// Contour plot of a function of two variables
#include <math.h>
#include "memalloc.h"
#include "graphlib.h"

#define pi 3.141592653589793

double Func(double x, double y)                     // function to be plotted
   { return cos(x*x) + cos(y*y); }

int main(int argc, wchar_t** argv)
{
   double **u;                            // declare dynamic array as pointer
   double hx, hy, umin, umax, x, xmin, xmax, y, ymin, ymax;
   int i, j, nx, ny;

   xmin = -pi; xmax = pi; ymin = -pi; ymax = pi;         // domain boundaries
   nx = 41; ny = 41;                                 // number of mesh points

   u = Matrix(1,nx,1,ny);                // allocate array of function values

   hx = (xmax-xmin)/(nx-1);                                 // x-mesh spacing
   hy = (ymax-ymin)/(ny-1);                                 // y-mesh spacing
   for (i=1; i<=nx; i++) {
      x = xmin + (i-1)*hx;                                    // x-mesh point
      for (j=1; j<=ny; j++) {
         y = ymin + (j-1)*hy;                                 // y-mesh point
         u[i][j] = Func(x,y);                               // function value
      }
   }

   umin = umax = u[1][1];                  // minimum and maximum of function
   for (i=1; i<=nx; i++)
      for (j=1; j<=ny; j++) {
         if (u[i][j] < umin) umin = u[i][j];
         if (u[i][j] > umax) umax = u[i][j];
      }

   PyGraph w(argc, argv);                       // create Tkinter root widget
   w.GraphInit(800,800);                                     // create canvas

   w.Contour(u,nx,ny,xmin,xmax,ymin,ymax,umin,umax,    // create contour plot
             0.15,0.85,0.15,0.85,"x","y","cos(x*x) + cos(y*y)");

   w.MainLoop();                                  // enter Tkinter event loop
}

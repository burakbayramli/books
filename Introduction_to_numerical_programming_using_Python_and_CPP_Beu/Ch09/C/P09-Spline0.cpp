// Interpolation with cubic splines
#include <stdio.h>
#include <math.h>
#include "memalloc.h"
#include "modfunc.h"

double sinc(double x) { return (x ? sin(x)/x : 1e0); }

int main()
{
   double *a, *b, *c, *d, *x, *y;
   double h, xi, xmin, xmax, yi, y1, y2;
   int i, ip, n, ni;
   FILE *out;

   xmin = 0e0; xmax = 5*asin(1e0);         // tabulation interval: [0,5*pi/2]
   n  = 6;                                           // number of data points
   ni = 100;                                // number of interpolation points

   a = Vector(1,n); b = Vector(1,n);                   // spline coefficients
   c = Vector(1,n); d = Vector(1,n);
   x = Vector(1,n); y = Vector(1,n);                           // data points

   h = (xmax-xmin)/(n-1);                             // generate data points
   for (i=1; i<=n; i++) {
      xi = xmin + (i-1)*h;
      x[i] = xi; y[i] = sinc(xi);
   }

   Spline(x,y,n,0e0,0e0,0,a,b,c,d,x,y,0);                  // natural splines

   out = fopen("interpol.txt","w");                       // open output file
   fprintf(out,"%s%s\n","     x           y           xi          yi     ",
                        "     y1          y2          f");
   h = (xmax-xmin)/(ni-1);
   for (i=1; i<=ni; i++) {
      xi = xmin + (i-1)*h;                         // interpolation arguments
      ip = 1; while (ip < n-1 && xi > x[ip+1]) ip++;       // index of spline

      yi = ((a[ip]*xi + b[ip])*xi + c[ip])*xi + d[ip];              // spline
      y1 = (3*a[ip]*xi + 2*b[ip])*xi + c[ip];               // 1st derivative
      y2 = 6*a[ip]*xi + 2*b[ip];                            // 2nd derivative

      if (i <= n)
         fprintf(out,"%12.3e%12.3e%12.3e%12.3e%12.3e%12.3e%12.3e\n",
                 x[i],y[i],xi,yi,y1,y2,sinc(xi));
      else
         fprintf(out,"%36.3e%12.3e%12.3e%12.3e%12.3e\n",xi,yi,y1,y2,sinc(xi));
   }
   fclose(out);
}

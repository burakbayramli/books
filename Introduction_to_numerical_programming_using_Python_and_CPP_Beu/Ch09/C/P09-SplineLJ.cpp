// Cubic spline interpolation of Lennard-Jones potential
#include <stdio.h>
#include "memalloc.h"
#include "modfunc.h"
                                                   // Lennard-Jones potential
double func(double x) { return 1e0/pow(x,12)-1e4/pow(x,6); }

int main()
{
   double h;
   double *a, *b, *c, *d, *x, *xi, *y, *yi;
   int i, n, ni;
   FILE *out;

   n  = 7;                                           // number of data points
   ni = 100;                                // number of interpolation points

   a = Vector(1,n); b = Vector(1,n);                   // spline coefficients
   c = Vector(1,n); d = Vector(1,n);
   x = Vector(1,n); y = Vector(1,n);              // interpolated data points
   xi = Vector(1,ni); yi = Vector(1,ni);              // interpolation points

   x[1] = 0.21; x[2] = 0.24; x[3] = 0.27; x[4] = 0.30;
   x[5] = 0.33; x[6] = 0.36; x[7] = 0.39;
   for (i=1; i<=n; i++) y[i] = func(x[i]);

   h = (x[n]-x[1])/(ni-1);
   for (i=1; i<=ni; i++) xi[i] = x[1] + (i-1)*h;   // interpolation arguments

   Spline(x,y,n,0e0,0e0,0,a,b,c,d,xi,yi,ni);               // natural splines

   out = fopen("interpol.txt","w");                       // open output file
   fprintf(out,"     x           y           xi          yi          f\n");
   for (i=1; i<=ni; i++) {
      if (i <= n)
         fprintf(out,"%12.3e%12.3e%12.3e%12.3e%12.3e\n",
                 x[i],y[i],xi[i],yi[i],func(xi[i]));
      else
         fprintf(out,"%36.3e%12.3e%12.3e\n",xi[i],yi[i],func(xi[i]));
   }
   fclose(out);
}

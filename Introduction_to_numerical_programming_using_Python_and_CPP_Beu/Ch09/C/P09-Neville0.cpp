// Neville interpolation
#include <stdio.h>
#include "memalloc.h"
#include "modfunc.h"

int main()
{
   double err, h, xi, yi;
   double *x, *y;
   int i, n, ni;
   FILE *out;

   n  = 8;                                           // number of data points
   ni = 50;                                 // number of interpolation points

   x = Vector(1,n);                                            // data points
   y = Vector(1,n);

   x[1] = 0.15; x[2] = 0.2; x[3] = 0.3; x[4] = 0.5;           // data points:
   x[5] = 0.8 ; x[6] = 1.1; x[7] = 1.4; x[8] = 1.7;           // f(x) = 1/x
   for (i=1; i<=n; i++) y[i] = 1e0/x[i];

   out = fopen("interpol.txt","w");                       // open output file
   fprintf(out,"     x           y           xi          yi         err\n");
   h = (x[n]-x[1])/(ni-1);
   for (i=1; i<=ni; i++) {
      xi = x[1] + (i-1)*h;                          // interpolation argument
      yi = Neville(x,y,n,xi,err);                        // interpolant value
      if (i <= n)
         fprintf(out,"%12.3e%12.3e%12.3e%12.3e%12.3e\n",
                 x[i],y[i],xi,yi,err);
      else
         fprintf(out,"%36.3e%12.3e%12.3e\n",xi,yi,err);
   }
   fclose(out);
}

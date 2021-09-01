// Monte Carlo calculation of the unit circle area
#include <math.h>
#include "memalloc.h"
#include "random.h"
#include "modfunc.h"
#include "graphlib.h"

int main(int argc, wchar_t** argv)
{
   double *sig, *x1, *x2, *y1, *y2;
   double a, b, chi2, fi, s, sigma, sigmb, x, y;
   long i, ip, n, ni, np;
   FILE *out;

   np = 400;                                     // number of plotting points
   x1 = Vector(1,np); y1 = Vector(1,np);                   // plotting points
   x2 = Vector(1,np); y2 = Vector(1,np); sig = Vector(1,np);

   seed();

   out = fopen("mcarlo.txt","w");                         // open output file
   fprintf(out,"     n       Int       sig      Int_w     sig_w\n");

   for (ip=1; ip<=np; ip++) {
      n = 250 * ip;                              // number of sampling points

      ni = 0;                                    // number of interior points
      for (i=1; i<=n; i++) {
         x = random(); y = random();
         if (x*x + y*y <= 1e0) ni++;                    // add interior point
      }
      fi = (double) ni/n;                      // fraction of interior points
      s = 4e0 * fi;                                               // integral
      sigma = 4e0 * sqrt((fi - fi*fi)/n);               // standard deviation
      fprintf(out,"%8d%10.5f%10.5f",n,s,sigma);
      x1[ip] =       double(n) ; y1[ip] = s;
      x2[ip] = log10(double(n)); y2[ip] = log10(sigma);
   }
   fclose(out);
                                                         // linear regression
   LinFit(x2,y2,sig,np,0,a,b,sigma,sigmb,chi2);
   printf("sigma = %6.3f n**(%6.3f)  w(x) = 1\n",pow(10e0,b),a);

   PyGraph c(argc, argv);
   c.GraphInit(1200,600);

   c.Plot(x1,y1,np,"blue",0,0.10,0.45,0.15,0.85,"n","Int",
          "Monte Carlo - unit circle area");

   c.Plot(x2,y2,np,"blue",0,0.60,0.95,0.15,0.85,"log(n)","log(sig)",
          "Monte Carlo - standard deviation");

   c.MainLoop();
}

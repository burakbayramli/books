// One-dimensional Monte Carlo quadrature with variance reduction
#include <math.h>
#include "memalloc.h"
#include "random.h"
#include "modfunc.h"
#include "graphlib.h"

//===========================================================================
double ranSqrt(double &w)
//---------------------------------------------------------------------------
// Returns a real random number x in the range [0,1) with the distribution 
// w(x) = 3/2 x^(1/2), and the corresponding value w(x)
//---------------------------------------------------------------------------
{
   double x;

   x = pow(random(),2e0/3e0);
   w = 1.5e0 * sqrt(x);
   return x;
}

double func(double x) { return x*exp(-x); }                      // integrand

int main(int argc, wchar_t** argv)
{
   double *sig, *x1, *x2, *y1, *y2;
   double a, b, chi2, f, f1, f2, s, sigma, sigmb, w, x;
   long i, ip, n, np;
   int nn[3], sty[3];                   // ending indexes and styles of plots
   const char* col[3];                                     // colors of plots
   FILE *out;

   np = 400;                                     // number of plotting points
   x1 = Vector(1,2*np); y1 = Vector(1,2*np);               // plotting points
   x2 = Vector(1,2*np); y2 = Vector(1,2*np); sig = Vector(1,2*np);

//   seed();

   out = fopen("mcarlo.txt","w");                         // open output file
   fprintf(out,"     n       Int       sig      Int_w     sig_w\n");

   for (ip=1; ip<=np; ip++) {
      n = 250 * ip;                              // number of sampling points

      f1 = f2 = 0e0;                      // quadrature with uniform sampling
      for (i=1; i<=n; i++) {
         x = random();              // RNG with uniform distribution in [0,1)
         f = func(x);                                            // integrand
         f1 += f; f2 += f * f;                                        // sums
      }
      f1 /= n; f2 /= n;                                           // averages
      s = f1;                                                     // integral
      sigma = sqrt((f2-f1*f1)/n);                       // standard deviation
      fprintf(out,"%8d%10.5f%10.5f",n,s,sigma);
      x1[ip] =       double(n) ; y1[ip] = s;
      x2[ip] = log10(double(n)); y2[ip] = log10(sigma);

      f1 = f2 = 0e0;                   // quadrature with importance sampling
      for (i=1; i<=n; i++) {
         x = ranSqrt(w);                        // RNG with distribution w(x)
         if (w) {
            f = func(x) / w;                                     // integrand
            f1 += f; f2 += f * f;                                     // sums
         }
      }
      f1 /= n; f2 /= n;                                           // averages
      s = f1;                                                     // integral
      sigma = sqrt((f2-f1*f1)/n);                       // standard deviation
      fprintf(out,"%10.5f%10.5f\n",s,sigma);
      x1[np+ip] =       double(n) ; y1[np+ip] = s;
      x2[np+ip] = log10(double(n)); y2[np+ip] = log10(sigma);
   }
   fclose(out);
                                                         // linear regression
   LinFit(x2,y2,sig,np,0,a,b,sigma,sigmb,chi2);
   printf("sigma = %6.3f n**(%6.3f)  w(x) = 1\n",pow(10e0,b),a);

   LinFit(&x2[np],&y2[np],sig,np,0,a,b,sigma,sigmb,chi2);
   printf("sigma = %6.3f n**(%6.3f)  w(x) = (3/2) x**(1/2)\n",pow(10e0,b),a);

   PyGraph c(argc, argv);
   c.GraphInit(1200,600);

   nn[1] =   np; col[1] = "blue"; sty[1] = 0;             // uniform sampling
   nn[2] = 2*np; col[2] = "red" ; sty[2] = 0;          // importance sampling
   c.MultiPlot(x1,y1,y1,nn,col,sty,2,10,0e0,0e0,0,0e0,0e0,0,
               0.10,0.45,0.15,0.85,"n","Int","Monte Carlo - integral");

   c.MultiPlot(x2,y2,y2,nn,col,sty,2,10,0e0,0e0,0,0e0,0e0,0,
               0.60,0.95,0.15,0.85,"log(n)","log(sig)",
               "Monte Carlo - standard deviation");

   c.MainLoop();
}

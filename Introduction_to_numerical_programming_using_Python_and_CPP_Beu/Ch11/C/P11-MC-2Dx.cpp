// Two-dimensional Monte Carlo quadrature with variance reduction
#include <math.h>
#include "memalloc.h"
#include "random.h"
#include "modfunc.h"
#include "graphlib.h"

double func(double x, double y)                                  // integrand
{
#define pi4 12.566370614359172
   return (x*x + y*y)*exp(-0.5e0*(x*x + y*y))/pi4;
}

int main(int argc, wchar_t** argv)
{
   double *sig, *x1, *x2, *y1, *y2;
   double a, b, chi2, f, f1, f2, L, L2, s, sigma, sigmb, w, x, y;
   long i, ip, n, np;
   int nn[4], sty[4];                   // ending indexes and styles of plots
   const char* col[4];                                     // colors of plots
   FILE *out;

   L = 8e0;                             // integration domain [-L,L] x [-L,L]
   L2 = L * L;                                     // area of sampling domain

   np = 400;                                     // number of plotting points
   x1 = Vector(1,3*np); y1 = Vector(1,3*np);               // plotting points
   x2 = Vector(1,3*np); y2 = Vector(1,3*np); sig = Vector(1,3*np);

   seed();

   out = fopen("mcarlo.txt","w");                         // open output file
   fprintf(out,"     n       Int       sig      Int_w     sig_w\n");

   for (ip=1; ip<=np; ip++) {
      n = 250 * ip;                              // number of sampling points

      f1 = f2 = 0e0;                      // quadrature with uniform sampling
      for (i=1; i<=n; i++) {
         x = L * random(); y = L * random();
         f = func(x,y);                                          // integrand
         f1 += f; f2 += f * f;                                        // sums
      }
      f1 /= n; f2 /= n;                                           // averages
      s = 4e0 * L2 * f1;                                          // integral
      sigma = 4e0 * L2 * sqrt((f2-f1*f1)/n);            // standard deviation
      fprintf(out,"%8d%10.5f%10.5f",n,s,sigma);
      x1[ip] =       double(n) ; y1[ip] = s;
      x2[ip] = log10(double(n)); y2[ip] = log10(sigma);

      f1 = f2 = 0e0;                     // quadrature with Gaussian sampling
      for (i=1; i<=n; i++) {
         randNrm2(w,x,y);          // random numbers with normal distribution
         f = func(x,y) / w;                                      // integrand
         f1 += f; f2 += f * f;                                        // sums
      }
      f1 /= n; f2 /= n;                                           // averages
      s = f1;                                                     // integral
      sigma = sqrt((f2-f1*f1)/n);                       // standard deviation
      fprintf(out,"%10.5f%10.5f",s,sigma);
      x1[np+ip] =       double(n) ; y1[np+ip] = s;
      x2[np+ip] = log10(double(n)); y2[np+ip] = log10(sigma);

      f1 = f2 = 0e0;                  // quadrature with exponential sampling
      for (i=1; i<=n; i++) {
         x = randExp();     // random variables with exponential distribution
         y = randExp();
         w = exp(-(x+y));
         f = func(x,y) / w;                                      // integrand
         f1 += f; f2 += f * f;                                        // sums
      }
      f1 /= n; f2 /= n;                                           // averages
      s = 4e0 * f1;                                               // integral
      sigma = 4e0 * sqrt((f2-f1*f1)/n);                 // standard deviation
      fprintf(out,"%10.5f%10.5f\n",s,sigma);
      x1[2*np+ip] =       double(n) ; y1[2*np+ip] = s;
      x2[2*np+ip] = log10(double(n)); y2[2*np+ip] = log10(sigma);
   }
   fclose(out);
                                                         // linear regression
   LinFit(x2,y2,sig,np,0,a,b,sigma,sigmb,chi2);
   printf("sigma ~ %6.3f n**(%6.3f)  uniform sampling\n",pow(10e0,b),a);

   LinFit(&x2[np],&y2[np],sig,np,0,a,b,sigma,sigmb,chi2);
   printf("sigma ~ %6.3f n**(%6.3f)  Gaussian sampling\n",
          pow(10e0,b),a);

   LinFit(&x2[2*np],&y2[2*np],sig,np,0,a,b,sigma,sigmb,chi2);
   printf("sigma ~ %6.3f n**(%6.3f)  exponential sampling\n",
          pow(10e0,b),a);

   PyGraph c(argc, argv);
   c.GraphInit(1200,600);

   nn[1] =   np; col[1] = "blue" ; sty[1] = 0;             // uniform sampling
   nn[2] = 2*np; col[2] = "red"  ; sty[2] = 0;            // Gaussian sampling
   nn[3] = 3*np; col[3] = "green"; sty[3] = 0;         // exponential sampling
   c.MultiPlot(x1,y1,y1,nn,col,sty,3,10,0e0,0e0,0,0e0,0e0,0,
               0.10,0.45,0.15,0.85,"n","Int","Monte Carlo - integral");

   c.MultiPlot(x2,y2,y2,nn,col,sty,3,10,0e0,0e0,0,0e0,0e0,0,
               0.60,0.95,0.15,0.85,"log(n)","log(sig)",
               "Monte Carlo - standard deviation");

   c.MainLoop();
}

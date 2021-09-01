// Two-dimensional Monte Carlo quadrature with variance reduction
#include <stdio.h>
#include <math.h>
#include "random.h"

double func(double x, double y)                                  // integrand
{
#define pi4 12.566370614359172
   return (x*x + y*y)*exp(-0.5e0*(x*x + y*y))/pi4;
}

int main()
{
   double f, f1, f2, L, L2, s, sigma, w, x, y;
   long i, n;

   L = 8e0;                             // integration domain [-L,L] x [-L,L]
   L2 = L * L;                                     // area of sampling domain

   n = 100000;                                   // number of sampling points

   seed();

   f1 = f2 = 0e0;                         // quadrature with uniform sampling
   for (i=1; i<=n; i++) {
      x = L * random(); y = L * random();
      f = func(x,y);                                             // integrand
      f1 += f; f2 += f * f;                                           // sums
   }
   f1 /= n; f2 /= n;                                              // averages
   s = 4e0 * L2 * f1;                                             // integral
   sigma = 4e0 * L2 * sqrt((f2-f1*f1)/n);               // standard deviation
   printf("Uniform sampling:  s = %f +/- %f\n",s,sigma);

   f1 = f2 = 0e0;                      // quadrature with importance sampling
   for (i=1; i<=n; i++) {
      randNrm2(w,x,y);             // random numbers with normal distribution
      f = func(x,y) / w;                                         // integrand
      f1 += f; f2 += f * f;                                           // sums
   }
   f1 /= n; f2 /= n;                                              // averages
   s = f1;                                                        // integral
   sigma = sqrt((f2-f1*f1)/n);                          // standard deviation
   printf("Gaussian sampling: s = %f +/- %f\n",s,sigma);
}

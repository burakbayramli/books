// One-dimensional Monte Carlo quadrature
#include <stdio.h>
#include <math.h>
#include "random.h"

double func(double x) { return x*exp(-x); }                      // integrand

int main()
{
   double f, f1, f2, s, sigma, x;
   long i, n;

   printf("n = "); scanf("%li",&n);              // number of sampling points

   seed();

   f1 = f2 = 0e0;                         // quadrature with uniform sampling
   for (i=1; i<=n; i++) {
      x = random();                          // RNG with uniform distribution
      f = func(x);                                               // integrand
      f1 += f; f2 += f * f;                                           // sums
   }
   f1 /= n; f2 /= n;                                              // averages
   s = f1;                                                        // integral
   sigma = sqrt((f2-f1*f1)/n);                          // standard deviation
   printf("s = %f +/- %f\n",s,sigma);
}

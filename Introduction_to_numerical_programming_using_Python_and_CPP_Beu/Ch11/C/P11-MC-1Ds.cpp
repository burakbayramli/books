// One-dimensional Monte Carlo quadrature with variance reduction
#include <stdio.h>
#include <math.h>
#include "random.h"

double randSqrt(double &w)
//---------------------------------------------------------------------------
// Returns a random number x in the range [0,1) with the distribution 
// w(x) = 3/2 x^(1/2), and the corresponding value w(x)
//---------------------------------------------------------------------------
{
   double x;

   x = pow(random(),2e0/3e0);
   w = 1.5e0 * sqrt(x);
   return x;
}

double func(double x) { return x*exp(-x); }                      // integrand

int main()
{
   double f, f1, f2, s, sigma, w, x;
   long i, n;

   printf("n = "); scanf("%li",&n);              // number of sampling points

   seed();

   f1 = f2 = 0e0;                      // quadrature with importance sampling
   for (i=1; i<=n; i++) {
      x = randSqrt(w);                          // RNG with distribution w(x)
      if (w) {
         f = func(x) / w;                                        // integrand
         f1 += f; f2 += f * f;                                        // sums
      }
   }
   f1 /= n; f2 /= n;                                              // averages
   s = f1;                                                        // integral
   sigma = sqrt((f2-f1*f1)/n);                          // standard deviation
   printf("s = %f +/- %f\n",s,sigma);
}

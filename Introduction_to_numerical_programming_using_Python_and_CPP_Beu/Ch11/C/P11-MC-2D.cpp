// Monte Carlo calculation of the unit circle area
#include <stdio.h>
#include <math.h>
#include "random.h"

int main()
{
   double fi, s, sigma, x, y;
   long i, n, ni;

   printf("n = "); scanf("%li",&n);              // number of sampling points

   seed();

   ni = 0;                                       // number of interior points
   for (i=1; i<=n; i++) {
      x = random(); y = random();
      if (x*x + y*y <= 1e0) ni++;                       // add interior point
   }
   fi = (double) ni/n;                         // fraction of interior points
   s = 4e0 * fi;                                                  // integral
   sigma = 4e0*sqrt((fi - fi*fi)/n);                    // standard deviation
   printf("Unit circle area = %f +/- %f\n",s,sigma);
}

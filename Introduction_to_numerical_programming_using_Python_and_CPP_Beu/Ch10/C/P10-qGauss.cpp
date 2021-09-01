// Gaussian quadratures
#include <stdio.h>
#include <math.h>
#include "integral.h"

double Func1(double x) { return 12*pow(x,11); }
double Func2(double x) { return pow(x,5) * exp(-x); }

int main()
{
   int n, nmax;

   nmax = 6;
   
   printf("Integral of 12*x^11 on [-1,1]\n");
   for (n=2; n<=nmax; n++) {
      printf("n = %i  I1 = %18.14g\n",n,qGaussLeg(Func1,0e0,1e0,n));
   }
   printf("\nIntegral of x^5 * exp(-x) on [0,+inf)\n");
   for (n=2; n<=nmax; n++) {
      printf("n = %i  I2 = %18.14g\n",n,qGaussLag(Func2,0e0,n));
   }
}

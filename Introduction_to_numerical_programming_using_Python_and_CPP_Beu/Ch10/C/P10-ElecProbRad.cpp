// Radial localization probability of an electron within the H atom
#include <stdio.h>
#include <math.h>
#include "integral.h"

double Func(double r)                    //  probability density for 2s state
{
   double f = r * (2-r);
   return f * f * exp(-r) / 8e0;
}

int main()
{
   double eps, I, R, Rinf;
   int n;
   
   eps = 1e-8;                             // relative precision of integrals

   printf("Radial localization probability\n");
   for (R = 10e0; R <= 30e0; R += 1e0) {
      I = qRomberg(Func,0e0,R,eps);
      printf("[0,%.1f] = %10.8f\n",R,I);
   }
   
   printf("\nTotal localization probability\n");
   Rinf = 25e0;                                        // guess for +infinity
   I = qImprop1(Func,0e0,Rinf,eps);
   printf("I Improp1  = %10.8f  Rinf = %.1f\n",I,Rinf);
   
   n = 3;                               // number of radial integration nodes
   I = qGaussLag(Func,0e0,n);
   printf("I GaussLag = %10.8f\n",I);
}

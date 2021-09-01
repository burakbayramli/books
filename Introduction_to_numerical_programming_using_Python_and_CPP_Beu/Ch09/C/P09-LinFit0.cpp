// Linear fit of a model to observed data points
#include <stdio.h>
#include "memalloc.h"
#include "modfunc.h"

int main()
{
   double *sigmy, *x, *y;
   double a, b, chi2, sigma, sigmb;
   int i, iopt, n;

   n = 5;                                          // number of observed data

   x = Vector(1,n); y = Vector(1,n);                         // observed data
   sigmy = Vector(1,n);               // standard deviations of observed data

   x[1] = 1e0; y[1] = 0.8e0;                                   // data points
   x[2] = 2e0; y[2] = 2.1e0;
   x[3] = 3e0; y[3] = 2.8e0;
   x[4] = 4e0; y[4] = 4.0e0;
   x[5] = 5e0; y[5] = 4.4e0;

   iopt = 0;                         // least squares fit: equal errors sigmy
   LinFit(x,y,sigmy,n,iopt,a,b,sigma,sigmb,chi2);

   printf("Least squares fit:\n");
   printf("a = %8.4f +/- %8.4f\n",a,sigma);
   printf("b = %8.4f +/- %8.4f\n",b,sigmb);
   printf("Chi^2 = %8.4f\n",chi2);

   for (i=1; i<=n; i++) sigmy[i] = 0.15*y[i]; // generate standard deviations

   iopt = 1;                        // Chi-square fit: different errors sigmy
   LinFit(x,y,sigmy,n,iopt,a,b,sigma,sigmb,chi2);

   printf("\nChi-square fit:\n");
   printf("a = %8.4f +/- %8.4f\n",a,sigma);
   printf("b = %8.4f +/- %8.4f\n",b,sigmb);
   printf("Chi^2 = %8.4f\n",chi2);
}

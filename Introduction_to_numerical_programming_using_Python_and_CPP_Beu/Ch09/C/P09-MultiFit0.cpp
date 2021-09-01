// Multilinear fit
#include <stdio.h>
#include <math.h>
#include "memalloc.h"
#include "modfunc.h"

void Func(double x, double func[], int npar)
{                            // returns the basis functions sin(x) and cos(x)
   func[1] = sin(x);
   func[2] = cos(x);
}

int main(int argc, wchar_t** argv)
{
   double *a, *func, *sigma, *sigmy, *x, *y;
   double chi2;
   int i, iopt, n, npar;

   n = 5;                                          // number of observed data
   npar = 2;                                    // number of model parameters

   x     = Vector(1,n);                     // x-coordinates of observed data
   y     = Vector(1,n);                     // y-coordinates of observed data
   sigmy = Vector(1,n);               // standard deviations of observed data
   func  = Vector(1,npar);                       // values of basis functions
   a     = Vector(1,npar);                                // model parameters
   sigma = Vector(1,npar);                     // uncertainties of parameters

   x[1] = 0e000; y[1] =  1e000;               // observed data generated from
   x[2] = 0.785; y[2] =  1.414;                     // f(x) = sin(x) + cos(x)
   x[3] = 1.571; y[3] =  1e000;
   x[4] = 2.356; y[4] =  0e000;
   x[5] = 3.141; y[5] = -1e000;

   iopt = 0;                         // least squares fit: equal errors sigmy
   MultiFit(x,y,sigmy,n,iopt,a,sigma,npar,chi2,Func);

   printf("Multilinear fit:\n");
   for(i=1; i<=npar; i++)
      printf("a[%i] = %8.4f +/- %8.4f\n",i,a[i],sqrt(sigma[i]));
   printf("Chi^2 = %8.4f\n",chi2);
}

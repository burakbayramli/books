// Polynomial fit
#include <stdio.h>
#include "memalloc.h"
#include "modfunc.h"
#include "graphlib.h"

int main(int argc, wchar_t** argv)
{
   double *a, *sigma, *sigmy, *x, *y;
   double chi2, f, h, xi;
   int i, iopt, j, n, nfit, npar;
   int nn[3], sty[3];                      // end-indexes and styles of plots
   const char* col[3];                                     // colors of plots

   n = 6;                                          // number of observed data
   npar = 4;                                    // number of model parameters
   nfit = 100;                         // number of points plotted from model

   x     = Vector(1,n+nfit);                // x-coordinates of observed data
   y     = Vector(1,n+nfit);                // y-coordinates of observed data
   sigmy = Vector(1,n+nfit);          // standard deviations of observed data
   a     = Vector(1,npar);                                // model parameters
   sigma = Vector(1,npar);                     // uncertainties of parameters

                 // observed data generated from P(x) = 0.1 x^3 - x^2 + 0.5 x
   x[1] = 0e0; y[1] =  0.0e0; x[2] =  2e0; y[2] =  -2.2e0;
   x[3] = 4e0; y[3] = -7.6e0; x[4] =  6e0; y[4] = -11.4e0;
   x[5] = 8e0; y[5] = -8.8e0; x[6] = 10e0; y[6] =   5.0e0;

   iopt = 0;                         // least squares fit: equal errors sigmy
   PolFit(x,y,sigmy,n,iopt,a,sigma,npar,chi2);

   printf("Polynomial fit:\n");
   for(i=1; i<=npar; i++)
      printf("a[%i] = %8.4f +/- %8.4f\n",i,a[i],sqrt(sigma[i]));
   printf("Chi^2 = %8.4f\n",chi2);

   h = (x[n]-x[1])/(nfit-1);
   for (i=1; i<=nfit; i++) {                           // append model points
      xi = x[1] + (i-1)*h;
      f = a[1];
      for (j=2; j<=npar; j++) f = f*xi + a[j];              // evaluate model
      x[n+i] = xi; y[n+i] = f;
   }

   PyGraph w(argc, argv);
   w.GraphInit(800,600);

   nn[1] = n       ; col[1] = "red" ; sty[1] = 4;            // observed data
   nn[2] = n + nfit; col[2] = "blue"; sty[2] = 1;             // fitted model

   w.MultiPlot(x,y,sigmy,nn,col,sty,2,10,0e0,0e0,0,0e0,0e0,0,
               0.15,0.95,0.15,0.85,"x","y","Polynomial fit");

   w.MainLoop();
}

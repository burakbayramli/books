// Multilinear fit
#include <stdio.h>
#include <math.h>
#include "memalloc.h"
#include "modfunc.h"
#include "graphlib.h"

void Func(double x, double func[], int npar)
{                            // returns the basis functions sin(x) and cos(x)
   func[1] = 1e0;
   func[2] = cos(x)  ; func[3] = sin(x);
   func[4] = cos(2*x); func[5] = sin(2*x);
}

int main(int argc, wchar_t** argv)
{
   double *a, *func, *sigma, *sigmy, *x, *y;
   double chi2, f, h, xi;
   int i, iopt, j, n, nfit, npar;
   int nn[3], sty[3];                      // end-indexes and styles of plots
   const char* col[3];                                     // colors of plots

   n = 9;                                          // number of observed data
   npar = 5;                                    // number of model parameters
   nfit = 100;                         // number of points plotted from model

   x     = Vector(1,n+nfit);                // x-coordinates of observed data
   y     = Vector(1,n+nfit);                // y-coordinates of observed data
   sigmy = Vector(1,n);               // standard deviations of observed data
   func  = Vector(1,npar);                       // values of basis functions
   a     = Vector(1,npar);                                // model parameters
   sigma = Vector(1,npar);                     // uncertainties of parameters

                 // observed data generated from f(x) = 1 - 2sin(x) + cos(2x)
   x[1] = 0.000; y[1] =  2.000; x[2] = 0.785; y[2] = -0.414;
   x[3] = 1.571; y[3] = -2.000; x[4] = 2.356; y[4] = -0.414;
   x[5] = 3.142; y[5] =  2.000; x[6] = 3.927; y[6] =  2.414;
   x[7] = 4.712; y[7] =  2.000; x[8] = 5.498; y[8] =  2.414;
   x[9] = 6.283; y[9] =  2.000;

   iopt = 0;                         // least squares fit: equal errors sigmy
   MultiFit(x,y,sigmy,n,iopt,a,sigma,npar,chi2,Func);

   printf("Multilinear fit:\n");
   for(i=1; i<=npar; i++)
      printf("a[%i] = %8.4f +/- %8.4f\n",i,a[i],sqrt(sigma[i]));
   printf("Chi^2 = %8.4f\n",chi2);

   h = (x[n]-x[1])/(nfit-1);
   for (i=1; i<=nfit; i++) {                           // append model points
      xi = x[1] + (i-1)*h;
      Func(xi,func,npar);                         // evaluate basis functions
      f = 0e0;
      for (j=1; j<=npar; j++) f += a[j]*func[j];            // evaluate model
      x[n+i] = xi; y[n+i] = f;
   }

   PyGraph w(argc, argv);
   w.GraphInit(800,600);

   nn[1] = n       ; col[1] = "red" ; sty[1] = 0;            // observed data
   nn[2] = n + nfit; col[2] = "blue"; sty[2] = 1;             // fitted model

   w.MultiPlot(x,y,sigmy,nn,col,sty,2,10,0e0,0e0,0,0e0,0e0,0,
               0.15,0.95,0.15,0.85,"x","y","Multilinear fit");

   w.MainLoop();
}

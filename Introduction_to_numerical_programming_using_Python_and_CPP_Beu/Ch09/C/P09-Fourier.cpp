// Fourier analysis using multilinear fit
#include <stdio.h>
#include <math.h>
#include "memalloc.h"
#include "modfunc.h"
#include "graphlib.h"

#define pi 3.141592653589793

void Func0(double x, double func[], int npar)         // sine basis functions
{
   int i;
   for (i=1; i<=npar; i++) func[i] = sin(i*x);
}

void Func(double x, double func[], int npar)       // Fourier basis functions
{                                                               // npar = odd
   double cosi, cosx, cos1, sini, sinx, sin1;
   int i;

   cosx = cos(x); cos1 = 1e0;
   sinx = sin(x); sin1 = 0e0;

   func[1] = 1e0;
   for (i=2; i<=npar-1; i+=2) {
      cosi = cosx * cos1 - sinx * sin1;
      sini = sinx * cos1 + cosx * sin1;
      cos1 = cosi; sin1 = sini;

      func[i] = cosi; func[i+1] = sini;
   }
}

int main(int argc, wchar_t** argv)
{
   double *a, *func, *sigma, *sigmy, *x, *y;
   double chi2, f, h, xi, xmin, xmax;
   int i, iopt, j, n, nfit, npar;
   int nn[3], sty[3];                      // end-indexes and styles of plots
   const char* col[3];                                     // colors of plots

   n = 500;                                        // number of observed data
   npar = 51;                                   // number of model parameters
   nfit = n;                           // number of points plotted from model

   x     = Vector(1,n+nfit);                // x-coordinates of observed data
   y     = Vector(1,n+nfit);                // y-coordinates of observed data
   sigmy = Vector(1,n);               // standard deviations of observed data
   func  = Vector(1,npar);                       // values of basis functions
   a     = Vector(1,npar);                                // model parameters
   sigma = Vector(1,npar);                     // uncertainties of parameters

   xmin = -pi; xmax = pi;                             // generate data points
   h = (xmax-xmin)/(n-1);
   for (i=1; i<=n; i++) {
      x[i] = xmin + (i-1)*h;
      y[i] = (x[i] < 0e0 ? -1e0 : 1e0 );            // periodic step function
//    y[i] = x[i];                                     // "sawtooth" function
//    y[i] = x[i] - pi/2;                      // shifted "sawtooth" function
   }

   iopt = 0;                         // least squares fit: equal errors sigmy
   MultiFit(x,y,sigmy,n,iopt,a,sigma,npar,chi2,Func0);

   h = (x[n]-x[1])/(nfit-1);
   for (i=1; i<=nfit; i++) {                           // append model points
      xi = x[1] + (i-1)*h;
      Func0(xi,func,npar);                        // evaluate basis functions
      f = 0e0;
      for (j=1; j<=npar; j++) f += a[j]*func[j];            // evaluate model
      x[n+i] = xi; y[n+i] = f;
   }

   PyGraph w(argc, argv);
   w.GraphInit(1200,600);

   nn[1] = n       ; col[1] = "red" ; sty[1] = 1;            // observed data
   nn[2] = n + nfit; col[2] = "blue"; sty[2] = 1;             // fitted model
   w.MultiPlot(x,y,sigmy,nn,col,sty,2,10,0e0,0e0,0,0e0,0e0,0,
               0.10,0.45,0.15,0.85,"x","y","Multilinear fit");

   for (i=1; i<=npar; i++) x[i] = double(i);
   w.Plot(x,a,npar,"red",3,0.60,0.95,0.15,0.85,
          "n","a","Fourier coefficients");

   w.MainLoop();
}

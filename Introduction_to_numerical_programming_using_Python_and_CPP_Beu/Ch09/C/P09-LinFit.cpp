// Linear fit of a model to observed data points
#include <stdio.h>
#include "memalloc.h"
#include "modfunc.h"
#include "graphlib.h"

int main(int argc, wchar_t** argv)
{
   double *sigmy, *x, *y;
   double a, b, chi2, sigma, sigmb, h;
   int i, iopt, n, nfit, n1, n2, n3;
   int nn[4], sty[4];                   // ending indexes and styles of plots
   const char* col[4];                                     // colors of plots

   n = 5;                                          // number of observed data
   nfit = 2;                           // number of points plotted from model
   n1 = n + nfit; n2 = n + 2*nfit;                             // end indexes

   x = Vector(1,n2); y = Vector(1,n2);                       // observed data
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

   h = (x[n]-x[1])/(nfit-1);
   for (i=1; i<=nfit; i++) {                           // append model points
      x[n+i] = x[1] + (i-1)*h; 
      y[n+i] = a*x[n+i] + b;                               // regression line
   }

   for (i=1; i<=n; i++) sigmy[i] = 0.15*y[i]; // generate standard deviations

   iopt = 1;                        // Chi-square fit: different errors sigmy
   LinFit(x,y,sigmy,n,iopt,a,b,sigma,sigmb,chi2);

   printf("\nChi-square fit:\n");
   printf("a = %8.4f +/- %8.4f\n",a,sigma);
   printf("b = %8.4f +/- %8.4f\n",b,sigmb);
   printf("Chi^2 = %8.4f\n",chi2);

   for (i=1; i<=nfit; i++) {                           // append model points
      x[n1+i] = x[n+i];
      y[n1+i] = a*x[n+i] + b;                   // Chi-square regression line
   }

   PyGraph w(argc, argv);
   w.GraphInit(800,600);

   nn[1] = n ; col[1] = "black"; sty[1] =  4;                  // data points
   nn[2] = n1; col[2] = "red"  ; sty[2] = -1;            // least squares fit
   nn[3] = n2; col[3] = "blue" ; sty[3] =  1;               // Chi-square fit
   w.MultiPlot(x,y,sigmy,nn,col,sty,3,10,0.5e0,5.5e0,1,0e0,0e0,0,
               0.15,0.95,0.15,0.85,"x","y","Linear fit");

   w.MainLoop();
}

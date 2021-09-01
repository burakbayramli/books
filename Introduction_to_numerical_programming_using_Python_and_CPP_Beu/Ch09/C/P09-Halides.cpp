// Absorption maximum of halides by linear and non-linear regression
#include <stdio.h>
#include <math.h>
#include "memalloc.h"
#include "modfunc.h"
#include "graphlib.h"

double Func(double x, double a[], int npar)     // model function for MarqFit
{
   return a[1] * pow(x,a[2]);
}

int main(int argc, wchar_t** argv)
{
   double *a, *sigma, *sigmy, *x, *y;
   double chi2, h;
   int i, iopt, n, npar;
   int nfit = 100;                     // number of points plotted from model
   int nn[3], sty[3];                      // end-indexes and styles of plots
   const char* col[3];                                     // colors of plots
   FILE *inp;

   inp = fopen("halides.dat","r");                          // open data file
   if (inp == NULL) { printf("halides.dat missing !\n"); exit(1); }

   fscanf(inp,"%i%i",&n,&npar);     // number of observed data and parameters
                                                           // allocate arrays
   x     = Vector(1,n+nfit);                // x-coordinates of observed data
   y     = Vector(1,n+nfit);                // y-coordinates of observed data
   sigmy = Vector(1,n);               // standard deviations of observed data
   a     = Vector(1,npar);                                // model parameters
   sigma = Vector(1,npar);                     // uncertainties of parameters

   fscanf(inp,"%i",&iopt);              // initialization option for sigmy[i]
   for (i=1; i<=npar; i++) fscanf(inp,"%lf",&a[i]);      // parameter guesses
   for (i=1; i<=n; i++) {
      fscanf(inp,"%lf%lf",&x[i],&y[i]);                      // observed data
      if (iopt) fscanf(inp,"%lf",&sigmy[i]);                   // uncertainty
   }
   fclose(inp);

   PyGraph w(argc, argv);
   w.GraphInit(1200,600);

   nn[1] = n       ; col[1] = "red" ; sty[1] = 4;            // observed data
   nn[2] = n + nfit; col[2] = "blue"; sty[2] = 1;             // fitted model

//---------------------------------------------------- Levenberg-Marquart fit
   a[1] = 1e3; a[2] = 1e0;                      // guess for model parameters
   MarqFit(x,y,sigmy,n,1,a,sigma,npar,chi2,Func);

   printf("Levenberg-Marquardt fit\n\n");
   printf("a = %6.2f +/- %6.2f\n",a[1],sigma[1]);
   printf("b = %6.2f +/- %6.2f\n",a[2],sigma[2]);
   printf("Chi^2 = %6.2f\n",chi2);

   h = (x[n]-x[1])/(nfit-1);
   for (i=1; i<=nfit; i++) {                           // append model points
      x[n+i] = x[1] + (i-1)*h;
      y[n+i] = Func(x[n+i],a,npar);                         // evaluate model
   }

   w.MultiPlot(x,y,sigmy,nn,col,sty,2,10,0e0,0e0,0,0e0,0e0,0,
               0.10,0.45,0.15,0.85,"d (A)","lam (A)",
               "Absorption maximum of halides - Levenberg-Marquardt fit");

//---------------------------------------------------------------- Linear fit
   for (i=1; i<=n; i++) sigmy[i] = log(1e0+sigmy[i]/y[i]);  // transform data
   for (i=1; i<=n+nfit; i++) { x[i] = log(x[i]); y[i] = log(y[i]); }

   LinFit(x,y,sigmy,n,1,a[1],a[2],sigma[1],sigma[2],chi2);

   printf("\nLinear fit\n\n");
   printf("a = %6.2f +/- %6.2f\n",exp(a[2]),exp(sigma[2]));
   printf("b = %6.2f +/- %6.2f\n",a[1],sigma[1]);

   for (i=1; i<=nfit; i++) y[n+i] = a[1] * x[n+i] + a[2];   // evaluate model

   w.MultiPlot(x,y,sigmy,nn,col,sty,2,10,0e0,0e0,0,0e0,0e0,0,
               0.60,0.95,0.15,0.85,"log(d)","log(lam)",
               "Absorption maximum of halides - Linear fit");

   w.MainLoop();
}

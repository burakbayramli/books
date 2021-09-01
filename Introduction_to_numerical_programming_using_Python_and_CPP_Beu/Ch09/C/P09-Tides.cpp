// Tidal analysis
#include <stdio.h>
#include <math.h>
#include "memalloc.h"
#include "modfunc.h"
#include "graphlib.h"

#define pi 3.141592653589793

const double T1 = 24.8412e0;                                  // lunar period
const double T2 = 24e0;                                       // solar period
const double omg1 = 2e0 * pi / (T1 / 2e0);           // lunar frequency (1/h)
const double omg2 = 2e0 * pi / (T2 / 2e0);           // solar frequency (1/h)

double Func1(double x, double a[], int npar)    // model function for MarqFit
{
   return a[1] + a[2] * cos(omg1 * x) + a[3] * sin(omg1 * x)
               + a[4] * cos(omg2 * x) + a[5] * sin(omg2 * x);
}

void Func2(double x, double func[], int npar)  // model function for MultiFit
{
   func[1] = 1e0;
   func[2] = cos(omg1 * x); func[3] = sin(omg1 * x);
   func[4] = cos(omg2 * x); func[5] = sin(omg2 * x);
}

int main(int argc, wchar_t** argv)
{
   double *a, *func, *sigma, *sigmy, *x, *y;
   double chi2, f, h, xi, yfit;
   int i, iopt, j, n, nfit, npar;
   int nn[3], sty[3];                      // end-indexes and styles of plots
   const char* col[3];                                     // colors of plots
   FILE *inp, *out;

   inp = fopen("tides.dat","r");                            // open data file
   if (inp == NULL) { printf("tides.dat missing !\n"); exit(1); }

   fscanf(inp,"%i%i",&n,&npar);     // number of observed data and parameters
   nfit = n;                           // number of points plotted from model
                                                           // allocate arrays
   x     = Vector(1,n+nfit);                // x-coordinates of observed data
   y     = Vector(1,n+nfit);                // y-coordinates of observed data
   sigmy = Vector(1,n);               // standard deviations of observed data
   func  = Vector(1,npar);                       // values of basis functions
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
   w.GraphInit(1200,800);

   nn[1] = n       ; col[1] = "red" ; sty[1] = 1;            // observed data
   nn[2] = n + nfit; col[2] = "blue"; sty[2] = 1;             // fitted model

//---------------------------------------------------- Levenberg-Marquart fit
   MarqFit(x,y,sigmy,n,iopt,a,sigma,npar,chi2,Func1);

   printf("Levenberg-Marquardt fit\n\n");
   for (i=1; i<=npar; i++)
      printf("a[%i] = %6.3f  sigma[%i] = %7.1e\n",i,a[i],i,sigma[i]);

   h = (x[n]-x[1])/(nfit-1);
   for (i=1; i<=nfit; i++) {                           // append model points
      x[n+i] = x[1] + (i-1)*h;
      y[n+i] = Func1(x[n+i],a,npar);
   }

   w.MultiPlot(x,y,sigmy,nn,col,sty,2,10,0e0,0e0,0,0e0,0e0,0,
               0.10,0.95,0.60,0.90,
               "t (h)","z (m)","Tidal analysis - Levenberg-Marquardt fit");

//----------------------------------------------------------- Multilinear fit
   MultiFit(x,y,sigmy,n,iopt,a,sigma,npar,chi2,Func2);

   printf("\nMultilinear fit\n\n");
   for (i=1; i<=npar; i++)
      printf("a[%i] = %6.3f  sigma[%i] = %7.1e\n",i,a[i],i,sigma[i]);

   h = (x[n]-x[1])/(nfit-1);
   for (i=1; i<=nfit; i++) {                           // append model points
      xi = x[1] + (i-1)*h;
      Func2(xi,func,npar);                        // evaluate basis functions
      f = 0e0;
      for (j=1; j<=npar; j++) f += a[j]*func[j];            // evaluate model
      x[n+i] = xi; y[n+i] = f;
   }

   w.MultiPlot(x,y,sigmy,nn,col,sty,2,10,0e0,0e0,0,0e0,0e0,0,
               0.10,0.95,0.10,0.40,
               "t (h)","z (m)","Tidal analysis - Multilinear fit");

   w.MainLoop();
}

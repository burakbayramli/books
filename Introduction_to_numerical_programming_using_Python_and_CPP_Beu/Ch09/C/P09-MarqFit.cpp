// Non-linear fit by the Levenberg-Marquardt method
#include <stdio.h>
#include <math.h>
#include "memalloc.h"
#include "modfunc.h"
#include "graphlib.h"

double Func(double x, double a[], int npar)           // f = sum of Gaussians
{                                                     // npar = multiple of 3
   double f;
   int i;

   f = 0e0;
   for (i=1; i<=npar; i+=3) f += a[i] * exp(-pow(x-a[i+1],2)/a[i+2]);
   return f;
}

int main(int argc, wchar_t** argv)
{
   double *a, *sigma, *sigmy, *x, *y;
   double chi2, h, yfit;
   int i, iopt, n, npar;
   int nfit = 100;                     // number of points plotted from model
   int nn[3], sty[3];                      // end-indexes and styles of plots
   const char* col[3];                                     // colors of plots
   FILE *inp, *out;

   inp = fopen("fit2.dat","r");                             // open data file
   if (inp == NULL) { printf("fit2.dat missing !\n"); exit(1); }

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

   out = fopen("fit.txt","w");                            // open output file
   fprintf(out,"n = %2i   npar = %2i\n",n,npar);
   fprintf(out,"Initial parameters:\n");
   for (i=1; i<=npar; i++) fprintf(out,"a[%i] = %6.3f\n",i,a[i]);
                                                    // Levenberg-Marquart fit
   MarqFit(x,y,sigmy,n,iopt,a,sigma,npar,chi2,Func);

   fprintf(out,"\nFinal parameters:\n");
   for (i=1; i<=npar; i++)
      fprintf(out,"a[%i] = %6.3f  sigma[%i] = %7.1e\n",i,a[i],i,sigma[i]);
   fprintf(out,"\nChi^2 = %7.1e\n",chi2);
   fprintf(out,"\n i      x         y       sigma      yfit     y-yfit\n");
   for (i=1; i<=n; i++) {
      yfit = Func(x[i],a,npar);       // model values for adjusted parameters
      fprintf(out,"%2i%10.5f%10.5f%10.5f%10.5f%10.1e\n",
                  i,x[i],y[i],sigmy[i],yfit,y[i]-yfit);
   }
   fclose(out);

   PyGraph w(argc, argv);
   w.GraphInit(800,600);
                                                            // prepare plots:
   nn[1] = n       ; col[1] = "red" ; sty[1] = 4;            // observed data
   nn[2] = n + nfit; col[2] = "blue"; sty[2] = 1;             // fitted model
   h = (x[n]-x[1])/(nfit-1);
   for (i=1; i<=nfit; i++) {                      // append data for 2nd plot
      x[n+i] = x[1] + (i-1)*h;
      y[n+i] = Func(x[n+i],a,npar);
   }

   w.MultiPlot(x,y,sigmy,nn,col,sty,2,10,0e0,0e0,0,0e0,0e0,0,
               0.15,0.95,0.15,0.85,"x","y","Non-linear fit");

   w.MainLoop();
}

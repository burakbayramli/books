// Non-linear fit by the Levenberg-Marquardt method
#include <stdio.h>
#include <math.h>
#include "memalloc.h"
#include "modfunc.h"

double Func(double x, double a[], int npar)                 // model function
{
   return a[1] * exp(-pow(x-a[2],2)/a[3]);
}

int main(int argc, wchar_t** argv)
{
   double *a, *sigma, *sigmy, *x, *y;
   double chi2, yfit;
   int i, iopt, n, npar;
   FILE *inp, *out;

   inp = fopen("fit.dat","r");                              // open data file
   if (inp == NULL) { printf("fit.dat missing !\n"); exit(1); }

   fscanf(inp,"%i%i",&n,&npar);     // number of observed data and parameters
                                                           // allocate arrays
   x     = Vector(1,n);                     // x-coordinates of observed data
   y     = Vector(1,n);                     // y-coordinates of observed data
   sigmy = Vector(1,n);               // standard deviations of observed data
   a     = Vector(1,npar);                                // model parameters
   sigma = Vector(1,npar);                     // uncertainties of parameters

   fscanf(inp,"%i",&iopt);              // initialization option for sigmy[i]
   for (i=1; i<=npar; i++) fscanf(inp,"%lf",&a[i]);      // parameter guesses
   for (i=1; i<=n; i++) {
      fscanf(inp,"%lf%lf",&x[i],&y[i]);
      if (iopt) fscanf(inp,"%lf",&sigmy[i]);                   // input sigmy
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
}

// Radial probability density in the hydrogen atom
#include <math.h>
#include "memalloc.h"
#include "specfunc.h"
#include "graphlib.h"

//===========================================================================
double Fact(int n)
//---------------------------------------------------------------------------
// Calculates the factorial of n
//---------------------------------------------------------------------------
{
   double f;
   int i;

   f = 1e0;
   for (i=2; i<=n; i++) f *= i;
   return f;
}

//===========================================================================
double RadPsiH(int n, int l, double r)
//---------------------------------------------------------------------------
// Evaluates the radial wave function of the hydrogen atom for principal
// quantum number n and orbital quantum number l at radius r (a0 is taken 1)
//---------------------------------------------------------------------------
{
   double fNorm;

   fNorm = sqrt(8e0/(n*n*n)*Fact(n-l-1)/(2e0*n*pow(Fact(n+l),3)));
     
   return fNorm * pow(r,l) * exp(-0.5e0*r) * aLaguerre(n+l,2*l+1,r);
}

int main(int argc, wchar_t** argv)
{
   int i, l, maxplotx, n, nmax, nplot, nplotx, nploty, nr;
   double dplotx, dploty, f, h, r, rmax, xplot, yplot;
   double fxmin, fxmax, fymin, fymax;
   double *x, *y;
   char title[20], nchr[5], lchr[5];
   
   PyGraph w(argc, argv);
   w.GraphInit(1200,800);

   nmax = 3;                                            // maximum value of n
   maxplotx = 3;                                 // max. no. of plots along x
   
   rmax = 50e0;
   h = 0.1e0;                                                    // x spacing
   nr = int(rmax/h) + 1;                                     // no. of points
   
   x = Vector(1,nr);
   y = Vector(1,nr);
   
   nplot = nmax*(nmax+1)/2;                             // total no. of plots
   nplotx = (nplot <= maxplotx ? nplot : maxplotx);   // no. of plots along x
   nploty = int(nplot/nplotx);                        // no. of plots along y
   if (nplot % nplotx) nploty++;                   // incomplete row of plots
   
   dplotx = 1e0/nplotx;                 // fractional width of a plot along x
   dploty = 1e0/nploty;                 // fractional width of a plot along y
   
   xplot = 0; yplot = 0;                         // lower-left corner of plot
   for (n=0; n<=nmax; n++) {                                        // n-loop
      for (l=0; l<n; l++) {                                         // l-loop
         for (i=1; i<=nr; i++) {                                    // r-loop
            r = (i-1) * h;
            f = RadPsiH(n,l,r);
            x[i] = r;
            y[i] = r * r * f * f;
         }
         
         fxmin = xplot + 0.20*dplotx; fxmax = xplot + 0.90*dplotx;  // viewport
         fymin = yplot + 0.15*dploty; fymax = yplot + 0.85*dploty;
         sprintf(nchr,"%i",n); sprintf(lchr,"%i",l);
         strcpy(title,"r^2 |R_");
         strcat(strcat(strcat(title,nchr),lchr),"(r)|^2");
         w.Plot(x,y,nr,"blue",1,fxmin,fxmax,fymin,fymax,"r","",title);
            
         xplot += dplotx;
         if (xplot >= 1) {                        // reached the right margin
            xplot = 0;                            // begin a new row of plots
            yplot += dploty;
         }
      }
   }
   w.MainLoop();
}

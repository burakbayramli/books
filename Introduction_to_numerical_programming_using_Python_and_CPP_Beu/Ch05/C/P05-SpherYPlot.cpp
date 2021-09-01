// Plot squared spherical harmonics
#include <math.h>
#include "memalloc.h"
#include "specfunc.h"
#include "graphlib.h"
#define pi 3.141592653589793

int main(int argc, wchar_t** argv)
{
   int i, l, lmax, m, maxplotx, n, nplot, nplotx, nploty;
   double dplotx, dploty, f, h, ReY, ImY, theta, xplot, yplot;
   double fxmin, fxmax, fymin, fymax;
   double *x, *y;
   char title[20], lchr[5], mchr[5];
   
   PyGraph w(argc, argv);
   w.GraphInit(1200,1000);

   lmax = 2;                                            // maximum value of l
   maxplotx = 3;                                 // max. no. of plots along x
   h = pi/180;                                         // theta angle spacing
   n = int(2*pi/h) + 1;                                // no. of theta values
   x = Vector(1,n); y = Vector(1,n);

   nplot = 0;
   for (l=0; l<=lmax; l++) nplot += (2*l+1);            // total no. of plots
   nplotx = (nplot <= maxplotx ? nplot : maxplotx);   // no. of plots along x
   nploty = int(nplot/nplotx);                        // no. of plots along y
   if (nplot % nplotx) nploty++;                   // incomplete row of plots
   
   dplotx = 1e0/nplotx;                 // fractional width of a plot along x
   dploty = 1e0/nploty;                 // fractional width of a plot along y
   
   xplot = 0; yplot = 0;                         // lower-left corner of plot
   for (l=0; l<=lmax; l++) {                                        // l-loop
      for (m=-l; m<=l; m++) {                                       // m-loop
         for (i=1; i<=n; i++) {                                 // theta-loop
            theta = i * h;
            SpherY(l,m,theta,0e0,ReY,ImY);              // spherical harmonic
            f = ReY * ReY + ImY * ImY;                        // squared norm
            x[i] = f * sin(theta);                   // Cartesian projections
            y[i] = f * cos(theta);
         }
         fxmin = xplot + 0.1*dplotx; fxmax = xplot + 0.9*dplotx;  // viewport
         fymin = yplot + 0.1*dploty; fymax = yplot + 0.9*dploty;
         sprintf(lchr,"%i",l); sprintf(mchr,"%i",m);
         strcpy(title,"l = ");
         strcat(strcat(strcat(title,lchr),",  m = "),mchr);
         w.Plot(x,y,n,"blue",2,fxmin,fxmax,fymin,fymax,"","",title);
            
         xplot += dplotx;
         if (xplot >= 1) {                        // reached the right margin
            xplot = 0;                            // begin a new row of plots
            yplot += dploty;
         }
      }
   }
   w.MainLoop();
}

// Fresnel integrals and Cornu spiral
#include <math.h>
#include "memalloc.h"
#include "integral.h"
#include "graphlib.h"
#define pi 3.141592653589793

double CosF(double u) { return cos(0.5e0*pi*u*u); }      // integrands of
double SinF(double u) { return sin(0.5e0*pi*u*u); }      // Fresnel integrals

int main(int argc, wchar_t** argv)
{
   int i, n;
   double eps, h, xmin, xmax;
   double *c, *s, *x;
   int sty[3], nn[3];
   const char* col[3]; 
   
   eps = 1e-6;                              // relative integration precision
   xmin = -3.5e0; xmax = 3.5e0;                   // interval of upper limits
   h = 0.05e0;                                       // plotting mesh spacing
   n = int((xmax-xmin)/h + 0.5) + 1;                // number of upper limits

   x = Vector(1,2*n); c = Vector(1,2*n); s = Vector(1,n);

   for (i=1; i<=n; i++) {
      x[i] = xmin + (i-1)*h; x[i+n] = x[i];                    // upper limit
      c[i] = qRomberg(CosF,0e0,x[i],eps);                // Fresnel integrals
      s[i] = qRomberg(SinF,0e0,x[i],eps); c[i+n] = s[i];
   }

   PyGraph w(argc, argv);
   w.GraphInit(1200,600);

   nn[1] = n  ; col[1] = "red" ; sty[1] =  1;
   nn[2] = 2*n; col[2] = "blue"; sty[2] = -1;                  // dashed line
   w.MultiPlot(x,c,c,nn,col,sty,2,10,
               0e0,0e0,0,0e0,0e0,0,0.10,0.45,0.15,0.85,
               "x","C, S","Fresnel integrals");
   w.Plot(c,s,n,"green",1,0.60,0.95,0.15,0.85,"C(x)","S(x)","Cornu Spiral");

   w.MainLoop();
}

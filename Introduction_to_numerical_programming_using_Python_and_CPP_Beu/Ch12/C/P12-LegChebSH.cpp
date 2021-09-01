// Legendre and Chebyshev polynomials by the shooting method
#include "memalloc.h"
#include "ode.h"
#include "graphlib.h"

int n;                                                    // polynomial order
double Func(double x, double y, double dy)             // RHS of Legendre ODE
   { return (2e0*x*dy - n*(n+1)*y) / (1e0 - x*x); }

double Func1(double x, double y, double dy)           // RHS of Chebyshev ODE
   { return (x*dy - n*n*y) / (1e0 - x*x); }

int main(int argc, wchar_t** argv)
{
   double *x, *xp, *y, *yp;
   double dy, dy1, dy2, eps, hx, P, xa, xb, ya, yb;
   int exist, m, m0, nmax, nx;
   int *nn, *sty;                          // end-indexes and styles of plots
   const char* col[10];                                    // colors of plots
   const char* color[] = {"blue", "cyan", "green", "orange", "red"};

   nmax = 5;                                      // max. order of polynomial
   xa = 0e0;                                               // boundary values
   xb = 1e0; yb = 1e0;
   eps = 1e-6;                                // tolerance for solution at xb
   hx = 1e-4;                                             // x-mesh step size

   nx = int((xb-xa)/hx + 0.5) + 1;                 // number of x-mesh points

   x = Vector(1,nx); y = Vector(1,nx);                    // x-mesh, solution
   xp = Vector(1,nx*nmax); yp = Vector(1,nx*nmax);         // plotting arrays
   nn = IVector(1,nmax);                           // ending indexes of plots
   sty = IVector(1,nmax);                                  // styles of plots

   for (m=1; m<=nx; m++) x[m] = xa + (m-1)*hx;             // generate x-mesh

   PyGraph w(argc, argv);
   w.GraphInit(1200,600);
                                                      // Legendre polynomials
   for (n=1; n<=nmax; n++) {                    // loop over polynomial order
      if (n % 2 == 0) {                          // even solutions: rescaling
         ya = 1e0; dy = 0e0;
         Propag(x,y,nx,ya,dy,Func);
         for (m=1; m<=nx; m++) y[m] /= y[nx];                // normalization
      } else {                                     // odd solutions: shooting
         ya = 0e0;
         dy1 = -1e3; dy2 = 1e3;     // search initial derivative in [dy1,dy2]
         dy = Shoot(x,y,nx,ya,yb,dy1,dy2,eps,exist,Func);
      }

      nn[n] = n*nx; col[n] = color[(n-1)%5]; sty[n] = 1;
      m0 = (n-1)*nx;
      for (m=1; m<=nx; m++) { xp[m0+m] = x[m]; yp[m0+m] = y[m]; }
   }

   w. MultiPlot(xp,yp,yp,nn,col,sty,nmax,10,0e0,0e0,0,-1e0,1e0,1,
                0.10,0.45,0.15,0.85,"x","Pn",
                "Legendre polynomials - shooting method");
                                                     // Chebyshev polynomials
   for (n=1; n<=nmax; n++) {                    // loop over polynomial order
      if (n % 2 == 0) {                          // even solutions: rescaling
         ya = 1e0; dy = 0e0;
         Propag(x,y,nx,ya,dy,Func1);
         for (m=1; m<=nx; m++) y[m] /= y[nx];                // normalization
      } else {                                     // odd solutions: shooting
         ya = 0e0;
         dy1 = -1e3; dy2 = 1e3;     // search initial derivative in [dy1,dy2]
         dy = Shoot(x,y,nx,ya,yb,dy1,dy2,eps,exist,Func1);
      }

      nn[n] = n*nx; col[n] = color[(n-1)%5]; sty[n] = 1;
      m0 = (n-1)*nx;
      for (m=1; m<=nx; m++) { xp[m0+m] = x[m]; yp[m0+m] = y[m]; }
   }

   w. MultiPlot(xp,yp,yp,nn,col,sty,nmax,10,0e0,0e0,0,-1e0,1e0,1,
                0.60,0.95,0.15,0.85,"x","Tn",
                "Chebyshev polynomials - shooting method");

   w.MainLoop();
}
// Legendre and Chebyshev polynomials by the finite-difference method
#include "memalloc.h"
#include "ode.h"
#include "graphlib.h"

int n;                                                    // polynomial order
void Func(double x, double &p, double &q, double &r)   // RHS of Legendre ODE
   { p = 2e0*x/(1e0-x*x); q =-n*(n+1)/(1e0-x*x); r = 0e0; }

void Func1(double x, double &p, double &q, double &r) // RHS of Chebyshev ODE
   { p = x/(1e0-x*x); q =-n*n/(1e0-x*x); r = 0e0; }

int main(int argc, wchar_t** argv)
{
   double *x, *xp, *y, *yp;
   double alf1, alf2, bet1, bet2, hx, xa, xb;
   int m, m0, nmax, nx;
   int *nn, *sty;                          // end-indexes and styles of plots
   const char* col[10];                                    // colors of plots
   const char* color[] = {"blue", "cyan", "green", "orange", "red"};

   nmax = 5;                                      // max. order of polynomial
   xa = -1e0;  xb = 1e0;                                 // domain boundaries
   hx = 1e-3;                                             // x-mesh step size

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
      alf1 = n % 2 ? -1e0 : 1e0; bet1 = 0e0;          // Dirichlet conditions
      alf2 = 1e0; bet2 = 0e0;
      Bilocal(xa,xb,y,nx,alf1,bet1,alf2,bet2,Func);

      nn[n] = n*nx; col[n] = color[(n-1)%5]; sty[n] = 1;
      m0 = (n-1)*nx;
      for (m=1; m<=nx; m++) { xp[m0+m] = x[m]; yp[m0+m] = y[m]; }
   }

   w. MultiPlot(xp,yp,yp,nn,col,sty,nmax,10,0e0,0e0,0,-1e0,1e0,1,
                0.10,0.45,0.15,0.85,"x","Pn",
                "Legendre polynomials - finite-differences");
                                                     // Chebyshev polynomials
   for (n=1; n<=nmax; n++) {                    // loop over polynomial order
      alf1 = n % 2 ? -1e0 : 1e0; bet1 = 0e0;          // Dirichlet conditions
      alf2 = 1e0; bet2 = 0e0;
      Bilocal(xa,xb,y,nx,alf1,bet1,alf2,bet2,Func1);

      nn[n] = n*nx; col[n] = color[(n-1)%5]; sty[n] = 1;
      m0 = (n-1)*nx;
      for (m=1; m<=nx; m++) { xp[m0+m] = x[m]; yp[m0+m] = y[m]; }
   }

   w. MultiPlot(xp,yp,yp,nn,col,sty,nmax,10,0e0,0e0,0,-1e0,1e0,1,
                0.60,0.95,0.15,0.85,"x","Tn",
                "Chebyshev polynomials - finite-differences");

   w.MainLoop();
}
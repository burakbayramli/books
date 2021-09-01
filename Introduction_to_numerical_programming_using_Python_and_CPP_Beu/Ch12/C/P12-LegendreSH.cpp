// Legendre polynomials by the shooting method
#include "memalloc.h"
#include "ode.h"
#include "specfunc.h"

int n;                                                    // polynomial order
double Func(double x, double y, double dy)             // RHS of Legendre ODE
   { return (2e0*x*dy - n*(n+1)*y) / (1e0 - x*x); }

int main()
{
   double *x, *y;
   double dy, dy1, dy2, eps, hx, P, xa, xb, ya, yb;
   int exist, m, nx;
   FILE *out;

   n = 5;                                     // order of Legendre polynomial
   xa = 0e0;                                               // boundary values
   xb = 1e0; yb = 1e0;
   eps = 1e-4;                                // tolerance for solution at xb
   hx = 1e-4;                                             // x-mesh step size

   nx = int((xb-xa)/hx + 0.5) + 1;                 // number of x-mesh points

   x = Vector(1,nx); y = Vector(1,nx);                    // x-mesh, solution

   for (m=1; m<=nx; m++) x[m] = xa + (m-1)*hx;             // generate x-mesh

   if (n % 2 == 0) {                             // even solutions: rescaling
      ya = 1e0; dy = 0e0;
      Propag(x,y,nx,ya,dy,Func);
      for (m=1; m<=nx; m++) y[m] /= y[nx];                   // normalization
   } else {                                        // odd solutions: shooting
      ya = 0e0;
      dy1 = -1e3; dy2 = 1e3;        // search initial derivative in [dy1,dy2]
      dy = Shoot(x,y,nx,ya,yb,dy1,dy2,eps,exist,Func);
   }

   out = fopen("shoot.txt","w");
   fprintf(out,"dy = %8.5f\n",dy);
   fprintf(out,"      x        P%i        err\n",n);
   for (m=1; m<=nx; m++) {
      P = Legendre(n,x[m],dy);
      fprintf(out,"%10.5f%10.5f%10.5f\n",x[m],y[m],P-y[m]);
   }
   fclose(out);
}
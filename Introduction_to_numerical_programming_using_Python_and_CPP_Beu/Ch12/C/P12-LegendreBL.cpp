// Legendre polynomials by the finite-difference method
#include "memalloc.h"
#include "ode.h"
#include "specfunc.h"

int n;                                                    // polynomial order
void Func(double x, double &p, double &q, double &r)   // RHS of Legendre ODE
   { p = 2e0*x/(1e0-x*x); q =-n*(n+1)/(1e0-x*x); r = 0e0; }

int main()
{
   double *x, *y;
   double alf1, alf2, bet1, bet2, d, hx, P, xa, xb;
   int m, nx;
   FILE *out;

   n = 5;                                     // order of Legendre polynomial
   xa = -1e0;  xb = 1e0;                                 // domain boundaries
   hx = 1e-3;                                             // x-mesh step size

   nx = int((xb-xa)/hx + 0.5) + 1;                 // number of x-mesh points

   x = Vector(1,nx); y = Vector(1,nx);                    // x-mesh, solution

   for (m=1; m<=nx; m++) x[m] = xa + (m-1)*hx;             // generate x-mesh

   alf1 = n % 2 ? -1e0 : 1e0; bet1 = 0e0;             // Dirichlet conditions
   alf2 = 1e0; bet2 = 0e0;

   Bilocal(xa,xb,y,nx,alf1,bet1,alf2,bet2,Func);

   out = fopen("bilocal.txt","w");
   fprintf(out,"      x        P%i        err\n",n);
   for (m=1; m<=nx; m++) {
      P = Legendre(n,x[m],d);
      fprintf(out,"%10.5f%10.5f%10.5f\n",x[m],y[m],P-y[m]);
   }
   fclose(out);
}
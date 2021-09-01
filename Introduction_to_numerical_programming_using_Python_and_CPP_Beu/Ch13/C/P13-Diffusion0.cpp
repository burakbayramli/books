// Solves the 1D diffusion equation by finite-difference schemes
#include "memalloc.h"
#include "pde.h"
#include "graphlib.h"
#define pi 3.141592653589793

double L;                                         // extent of spatial domain
void Init(double u[], double x[], int nx)
{                              // initial solution for the diffusion equation
   int i;
   for (i=1; i<=nx; i++) u[i] = sin(pi*x[i]/L);
}

int main(int argc, wchar_t** argv)
{
   double *u0, *u, *x;
   double D, f, ht, hx, t, tmax;
   int i, nx;
   FILE *out;

   D    = 0.1e0;                                     // diffusion coefficient
   L    = 1e0;                                        // [0,L] spatial domain
   nx   = 21;                                // number of spatial mesh points
   tmax = 6.0e0;                                          // propagation time
   ht   = 1.25e-2;                                               // time step

   u0 = Vector(1,nx); u = Vector(1,nx);                           // solution
   x = Vector(1,nx);                                          // spatial mesh

   hx = L/(nx-1);
   for (i=1; i<=nx; i++) x[i] = (i-1)*hx;                     // spatial mesh

   Init(u0,x,nx);                                         // initial solution

   t = 0e0;
   while (t < tmax) {                                        // temporal loop
      t += ht;                                               // increase time
      PropagFTCS(u0,u,nx,D,hx,ht);                      // propagate solution

      for (i=1; i<=nx; i++) u0[i] = u[i];                  // shift solutions
   }

   out = fopen("diffusion.txt","w");                      // open output file
   fprintf(out,"lambda = %f t = %f\n",D*ht/(hx*hx),t);
   fprintf(out,"      x         u       exact\n");
   f = exp(-pi*pi*D*t/(L*L));
   for (i=1; i<=nx; i++)
      fprintf(out,"%10.5f%10.5f%10.5f\n",x[i],u[i],f*sin(pi*x[i]/L));
   fclose(out);

   PyGraph w(argc, argv);
   w.GraphInit(800,600);
   w.Plot(x,u,nx,"blue",1,0.15,0.95,0.15,0.85,"x","u","Diffusion");
   w.MainLoop();
}

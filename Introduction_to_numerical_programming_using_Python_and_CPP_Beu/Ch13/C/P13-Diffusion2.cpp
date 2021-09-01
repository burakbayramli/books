// Solves the diffusion equation for spatially variable diffusion coefficient
#include "memalloc.h"
#include "pde.h"
#include "graphlib.h"

double L;                                         // extent of spatial domain
void Init(double u[], double x[], double D[], int nx)       // initialization
{
   double x1 = 0.4e0*L;                                   // limit of layer 1
   double x2 = 0.6e0*L;                                   // limit of layer 2
   double c = 0.5e0*(x1+x2);                             // center of layer 2
   double w = 0.5e0*(x2-x1);                         // half-width of layer 2
   double d = 0.01e0 * L;                 // drop distance of switch function
   double D1 = 0.1e0, D2 = 0.001e0;  // nominal D coef. in layers 1 (3) and 2
   double fsw;               // switch function: 1/0 inside/outside the layer
   int i;

   for (i=1; i<=nx; i++) {
      fsw = 1e0 / (1e0 + exp((fabs(x[i]-c)-w)/d));
      D[i] = D1 + (D2 - D1) * fsw;
      u[i] = x[i] <= x1 ? 1e0: 0e0;
   }
}

int main(int argc, wchar_t** argv)
{
   double *D, *u0, *u, *x;
   double ht, hx, Jdiff1, Jdiff2, t, tmax;
   int i, it, iopBC1, iopBC2, nout, nt, nx;
   char title[80];
   FILE *out;

   L      = 1e0;                                      // [0,L] spatial domain
   iopBC1 = 1; iopBC2 = 1;              // left/right boundary condition type
   Jdiff1 = 1e0; Jdiff2 = 1e0;                  // left/right boundary fluxes
   nx     = 101;                             // number of spatial mesh points
   tmax   = 2e0;                                          // propagation time
   ht     = 1e-3;                                                // time step
   nout   = 100;                                   // output every nout steps

   nt = (int)(tmax/ht + 0.5);                         // number of time steps
   u0 = Vector(1,nx); u = Vector(1,nx);                           // solution
   x = Vector(1,nx);                                          // spatial mesh
   D = Vector(1,nx);                                  // difusion coefficient

   hx = L/(nx-1);
   for (i=1; i<=nx; i++) x[i] = (i-1)*hx;                     // spatial mesh

   Init(u0,x,D,nx);        // initialization: diffusion coefficient, solution

   PyGraph w(argc, argv);
   w.GraphInit(1000,700);

   out = fopen("diffusion.txt","w");
   fprintf(out,"      x   ");
   for (i=1; i<=nx; i++) fprintf(out,"%10.5f",x[i]);          // print x-mesh
   fprintf(out,"\n");
   fprintf(out,"      t         u\n");

   for (it=1; it<=nt; it++) {                                    // time loop
      t = it*ht;                                        // propagate solution
      PropagDiff(u0,u,D,nx,hx,ht,iopBC1,iopBC2,Jdiff1,Jdiff2);

      for (i=1; i<=nx; i++) u0[i] = u[i];                  // shift solutions

      if (it % nout == 0 || it == nt) {            // output every nout steps
         fprintf(out,"%10.5f",t);
         for (i=1; i<=nx; i++) fprintf(out,"%10.5f",u[i]);
         fprintf(out,"\n");

         w.GraphClear();
         sprintf(title,"Diffusion  t = %4.2f",t);
         w.Plot(x,u,nx,"blue",1,0.15,0.95,0.50,0.90,"None","u",title);
         w.Plot(x,D,nx,"red",1,0.15,0.95,0.08,0.48,"x","D","");
         w.GraphUpdate();
      }
   }
   fclose(out);

   w.MainLoop();
}

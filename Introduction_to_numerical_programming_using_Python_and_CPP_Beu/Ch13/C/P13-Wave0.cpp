// Solve 1D wave equation by the explicit finite-difference method
#include "memalloc.h"
#include "pde.h"

//===========================================================================
void Init(double u0[], double u1[], double x[], int nx, double c, double dk,
          double hx, double ht)
//---------------------------------------------------------------------------
// Returns initial solutions u0 and u1, for the first two time steps
//    u0(x,0) = sin(x*dk) / (x*dk)                           initial solution
//    v0(x,0) = 0                                     initial time derivative
// x - spatial mesh, nx - number of nodes
// c - phase velocity of wave, dk - wave number interval
// hx - x-spacing, ht - time step size
//---------------------------------------------------------------------------
{
   double lam, lam2, v0;
   int i;

   for (i=1; i<=nx; i++)                                       // time step 0
      u0[i] = dk*x[i] ? sin(dk*x[i])/(dk*x[i]) : 1e0;     // initial solution

   lam = c*ht/hx; lam = lam*lam;                               // time step 1
   lam2 = 2e0*(1e0 - lam);
   u1[1] = u0[1]; u1[nx] = u0[nx];                // constant boundary values
   for (i=2; i<=nx-1; i++) {
      v0 = 0e0;                                    // initial time derivative
      u1[i] = 0.5e0*(lam*u0[i-1] + lam2*u0[i] + lam*u0[i+1]) - ht*v0;
   }
}

int main()
{
   double *u0, *u1, *u, *x;
   double c, dk, ht, hx, t, tmax, xmax;
   int i, it,  nout, nt, nx, nx2;
   char fname[80];
   FILE *out;

   c    = 10e0;                                        // phase speed of wave
   dk   = 1e0;                                        // wave number interval
   xmax = 100e0;                                                 // maximum x
   hx   = 5e-2;                                          // spatial step size
   tmax = 40e0;                                   // maximum propagation time
   ht   = 5e-3;                                                  // time step
   nout = 500;                                     // output every nout steps

   nx = 2*(int)(xmax/hx + 0.5) + 1;            // odd number of spatial nodes
   nt = (int)(tmax/ht + 0.5);                         // number of time steps
   nx2 = nx/2;

   u0 = Vector(1,nx);                                     // initial solution
   u1 = Vector(1,nx);                            // first propagated solution
   u  = Vector(1,nx);                                             // solution
   x  = Vector(1,nx);                                         // spatial mesh

   for (i=1; i<=nx; i++) x[i] = (i-nx2-1)*hx;                 // spatial mesh

   Init(u0,u1,x,nx,c,dk,hx,ht);                        // initial wave packet

   for (it=1; it<=nt; it++) {                                    // time loop
      t = it*ht;
      PropagWave(u0,u1,u,nx,c,hx,ht);                   // propagate solution
                                                           // shift solutions
      for (i=1; i<=nx; i++) { u0[i] = u1[i];  u1[i] = u[i]; }

      if (it % nout == 0 || it == nt) {            // output every nout steps
         sprintf(fname,"wave_%4.2f.txt",t);
         out = fopen(fname,"w");
         fprintf(out,"t = %4.2f\n",t);
         fprintf(out,"     x          u\n");
         for (i=1; i<=nx; i++)
            fprintf(out,"%10.5f%10.5f\n",x[i],u[i]);
         fclose(out);
      }
   }
}

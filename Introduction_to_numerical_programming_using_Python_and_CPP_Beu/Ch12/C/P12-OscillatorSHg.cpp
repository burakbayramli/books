// Eigenstates of the 1D Schroedinger equation for the harmonic oscillator
//    y" = 2 [V(x) - E] y, y(0) = y0, y'(0) = y'0, y(+inf) = 0
//    V(x) = 0.5*x*x
// using a shooting algorithm based on the Numerov method
//---------------------------------------------------------------------------
#include "memalloc.h"
#include "ode.h"
#include "graphlib.h"
                                         // Potential for harmonic oscillator
double Pot(double x) { return 0.5e0*x*x; } 

int main(int argc, wchar_t** argv)
{
   double *V, *x, *y, *y2;
   double dE, dy0, E, Ew, eps, f, fy, hx, hy, Vmin, Vmax, xc, xx, y0;
   int exist, iE, m, nc, nE, nx, par;
   char *xtext, title[80];

   xx = 10e0;                                              // limit of x-mesh
   xc = 6e0;                   // checkpoint for vanishing solution (xc < xx)
   nE = 8;                          // number of eigenvalues to be calculated
   dE = 0.1e0;                           // minimum separation of eigenvalues
   eps = 1e-4;                                // tolerance for solution at xc
   hx = 1e-3;                                             // x-mesh step size

   nx = int(xx/hx + 0.5) + 1;                      // number of x-mesh points
   nc = int(xc/hx + 0.5) + 1;          // index of checkpoint for vanishing y
   x = Vector(1,nx); y = Vector(1,nx); y2 = Vector(1,nx); // x-mesh, solution
   V = Vector(1,nx);                                   // tabulated potential

   Vmin = Vmax = Pot(xx);
   for (m=1; m<=nx; m++) {
      x[m] = (m-1)*hx;                                    // integration mesh
      V[m] = Pot(x[m]);                                 // tabulate potential
      if (Vmin > V[m]) Vmin = V[m];                      // potential minimum
      if (Vmax < V[m]) Vmax = V[m];                      // potential maximum
   }

   PyGraph w(argc, argv);
   w.GraphInit(800,800);

   hy = 0.92e0/nE;                              // fractional height of plots
   fy = 0.05;                           // lower fractional position of plots

   iE = 0;                                                // index of found E
   par = 0;                                         // parity of ground state
   Ew = Vmin;               // lower limit of search window for E, [Ew,Ew+dE]
   while (Ew < Vmax && iE < nE) {             // loop over eigenvalue windows
                                                   // initial values at x = 0
      if (par == 0) { y0 = 1e0; dy0 = 0e0; }                        // even y
      else          { y0 = 0e0; dy0 = 1e0; }                         // odd y
                                                       // shoot in [Ew,Ew+dE]
      E = ShootQM(Ew,Ew+dE,V,x,y,nx,nc,y0,dy0,eps,exist);

      Ew += dE;                            // shift [Ew,Ew+dE] for next shoot

      if (exist) {
         iE += 1;                                              // found new E
         par = par ? 0 : 1;                               // parity of next y

         f = 0e0;                          // normalize y by trapezoidal rule
         for (m=1; m<=nc; m++) f += y[m]*y[m]*hx;        // norm for [0,+inf]
         f = sqrt(2e0*f);
         if (int((iE-1)/2) % 2) f = -f;                    // sign correction
         for (m=1; m<=nc; m++) { y[m] /= f; y2[m] = y[m]*y[m]; }

         sprintf(title,"E%i = %4.2f",iE-1,E);
         xtext = (iE == 1) ? "x" : "None";
         w.Plot(x,y,nc,"blue",1,0.11,0.48,fy,fy+hy,xtext,"y",title);
         w.Plot(x,y2,nc,"blue",1,0.61,0.98,fy,fy+hy,xtext,"y^2",title);
         fy += hy;                      // fractional y-position of next plot
      }
   }
   w.MainLoop();
}

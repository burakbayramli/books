// Charged particle orbiting about a fixed charge - Euler PC method
#include "memalloc.h"
#include "ode.h"
#include "graphlib.h"

double m, q, Q;               // particle mass, particle charge, fixed charge
void Func(double t, double y[], double f[])          // RHS of 1st order ODEs
{
   double factCoul = 14.3996517e0;                 // e**2/(4*pi*eps0) [eV*A]
   double fr, r, r2;                        // mass: u, distance: A, time: ps
                                        // charge: e, energy: eV, force: eV/A
   r2 = y[1]*y[1] + y[2]*y[2];
   r = sqrt(r2);                                // distance from force center
   fr = factCoul * q * Q / r2;                                // radial force
   f[1] = y[3];
   f[2] = y[4];
   f[3] = fr/m * y[1]/r;
   f[4] = fr/m * y[2]/r;
}

int main(int argc, wchar_t** argv)
{
   double ht, t, tmax, vx0, vx, vy0, vy, x0, y0, *y, *tt, *xt, *yt;
   int it, n, nt;

   m = 1e0;                                                //mass of particle
   q = -1.e0; Q = 1.e0;                              // charges in units of e
   x0 = 1e0; y0 = 0e0;                                    // initial position
   vx0 = 0e0; vy0 = 4.5e0;                                // initial velocity
   tmax = 20e0;                                                  // time span
   ht = 0.01e0;                                             // time step size

   n = 4;                                         // number of 1st order ODEs
   nt = int(tmax/ht + 0.5) + 1;                       // number of time steps
   y = Vector(1,n);                                    // solution components
   tt = Vector(1,nt); xt = Vector(1,nt); yt = Vector(1,nt);// plotting arrays

   t = 0e0; it = 1;
   y[1] = x0; y[3] = vx0;                                   // initial values
   y[2] = y0; y[4] = vy0;
   tt[1] = t; xt[1] = y[1]; yt[1] = y[2];               // store for plotting

   while (t+ht <= tmax) {                                 // propagation loop
      EulerPC(t,ht,y,n,Func);
      t += ht; it += 1;

      tt[it] = t; xt[it] = y[1]; yt[it] = y[2];         // store for plotting
   }

   PyGraph w(argc, argv);
   w.GraphInit(1200,600);
   w.Plot(tt,xt,it,"blue",1,0.10,0.45,0.15,0.85,
          "t","x","x-position of charged particle");
   w.Plot(xt,yt,it,"blue",1,0.60,0.95,0.15,0.85,
          "x","y","Trajectory of charged particle");
   w.MainLoop();
}

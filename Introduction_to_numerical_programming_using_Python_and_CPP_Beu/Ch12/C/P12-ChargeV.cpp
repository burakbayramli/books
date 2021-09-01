// Charged particle orbiting about a fixed charge - Verlet method
#include "memalloc.h"
#include "ode.h"
#include "graphlib.h"

double m, q, Q;               // particle mass, particle charge, fixed charge
void Forces(double m, double x, double y, double vx, double vy,
            double &fx, double &fy, double &Epot)
{
   double factCoul = 14.3996517e0;              // e**2/(4*pi*eps0) [eV*Angs]
   double fr, r, r2;                        // mass: u, distance: A, time: ps
                                        // charge: e, energy: eV, force: eV/A
   r2 = x*x + y*y;
   r = sqrt(r2);                                // distance from force center
   fr = factCoul * q * Q / r2;                                // radial force
   fx = fr * x/r;                                         // force components
   fy = fr * y/r;
   Epot = fr * r;                                         // potential energy
}

int main(int argc, wchar_t** argv)
{
   double ax, ay, fx, fy, vx0, vx, vy0, vy, x0, x, y0, y;
   double Ekin, Epot, ht, t, tmax, *tt, *Et, *xt, *yt;
   int it, nt;
   int nn[4], sty[4];                   // ending indexes and styles of plots
   const char* col[4];                                     // colors of plots

   m = 1e0;                                                //mass of particle
   q = -1.e0; Q = 1.e0;                              // charges in units of e
   x0 = 1e0; y0 = 0e0;                                    // initial position
   vx0 = 0e0; vy0 = 4.5e0;                                // initial velocity
   tmax = 20e0;                                                  // time span
   ht = 0.1e0;                                              // time step size

   nt = int(tmax/ht + 0.5) + 1;                       // number of time steps
   tt = Vector(1,3*nt); Et = Vector(1,3*nt);               // plotting arrays
   xt = Vector(1,nt); yt = Vector(1,nt);

   t = 0e0; it = 1;                                    // Euler-Cromer method
   x = x0; vx = vx0;                                        // initial values
   y = y0; vy = vy0;

   Forces(m,x,y,vx,vy,fx,fy,Epot);     // initial forces and potential energy
   ax = fx/m; ay = fy/m;                              // initial acceleration
   Ekin = 0.5e0 * m * (vx*vx + vy*vy);              // initial kinetic energy
   xt[it] = x; yt[it] = y;                              // store for plotting
   tt[it] = tt[nt+it] = t; tt[2*nt+it] = t;
   Et[it] = Ekin; Et[nt+it] = Epot; Et[2*nt+it] = Ekin + Epot;

   while (t <= tmax) {                                    // propagation loop
      Verlet2(ht,m,x,y,vx,vy,ax,ay,Ekin,Epot,Forces);
      t += ht; it += 1;

      xt[it] = x; yt[it] = y;                           // store for plotting
      tt[it] = tt[nt+it] = t; tt[2*nt+it] = t;
      Et[it] = Ekin; Et[nt+it] = Epot; Et[2*nt+it] = Ekin + Epot;
   }

   PyGraph w(argc, argv);
   w.GraphInit(1200,600);

   nn[1] =   nt; col[1] = "blue" ; sty[1] = 1;                        // Ekin
   nn[2] = 2*nt; col[2] = "green"; sty[2] = 1;                        // Epot
   nn[3] = 3*nt; col[3] = "red"  ; sty[3] = 1;                        // Etot
   w.MultiPlot(tt,Et,Et,nn,col,sty,3,10,0e0,0e0,0,0e0,0e0,0,
               0.10,0.45,0.15,0.85,"t","Ek, Ep, Et","Particle energies");
   w.Plot(xt,yt,nt,"blue",1,0.60,0.95,0.15,0.85,
          "x","y","Particle trajectory");
   w.MainLoop();
}

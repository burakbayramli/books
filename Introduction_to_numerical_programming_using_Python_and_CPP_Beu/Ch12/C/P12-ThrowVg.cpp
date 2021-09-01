// Oblique throw of an object with drag using the velocity Verlet method
#include "memalloc.h"
#include "ode.h"
#include "graphlib.h"

#define pi 3.141592653589793
#define g 9.81e0                                // gravitational acceleration

double m, k;                          // mass of object, velocity coefficient
void Forces(double m, double x, double y, double vx, double vy,
            double &fx, double &fy, double &Epot)
{
   fx = -k * vx*abs(vx);                                  // force components
   fy = -k * vy*abs(vy) - m*g;
   Epot = m*g*y;                                          // potential energy
}

int main(int argc, wchar_t** argv)
{
   double *tt, *xt, *yt;
   double ax, ay, Ekin, Epot, ht, t, tmax, vx0, vx, vy0, vy, x0, x, y0, y;
   double Cd, phi, rho, R, v0;
   int it, nt;

   m = 7.26e0;                                              // mass of hammer
   R = 0.06e0;                                            // radius of hammer
   x0 = 0e0; y0 = 3e0;                                    // initial position
   v0 = 29e0;                                             // initial velocity
   phi = 45e0 * pi/180e0;                                      // throw angle
   vx0 = v0 *cos(phi); vy0 = v0*sin(phi);                 // initial velocity
   rho = 1.2;                                               // density of air
   Cd = 0.5e0;                                            // drag coefficient
   k = 0.5e0*rho*(pi*R*R)*Cd;                         // velocity coefficient
   tmax = 20e0;                                                  // time span
   ht = 0.001e0;                                            // time step size

   nt = int(tmax/ht + 0.5) + 1;                       // number of time steps
   tt = Vector(1,nt); xt = Vector(1,nt); yt = Vector(1,nt);// plotting arrays

   t = 0e0; it = 1;
   x = x0; vx = vx0; ax = 0e0;                              // initial values
   y = y0; vy = vy0; ay = 0e0;
   tt[1] = t; xt[1] = x; yt[1] = y;                     // store for plotting

   while (t+ht <= tmax) {                                 // propagation loop
      Verlet2(ht,m,x,y,vx,vy,ax,ay,Ekin,Epot,Forces);
      t += ht; it += 1;

      tt[it] = t; xt[it] = x; yt[it] = y;               // store for plotting
      if (y < 0.e0) break;                  // stop if object hits the ground
   }
   printf("xmax = %5.2f\n",x);

   PyGraph w(argc, argv);
   w.GraphInit(1200,600);
   w.Plot(tt,yt,it,"blue",1,0.10,0.45,0.15,0.85,"t","y","Altitude");
   w.Plot(xt,yt,it,"blue",1,0.60,0.95,0.15,0.85,"x","y","Trajectory");
   w.MainLoop();
}

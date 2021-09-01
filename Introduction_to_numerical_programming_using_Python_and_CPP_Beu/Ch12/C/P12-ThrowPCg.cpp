// Oblique throw of an object with drag using the Euler PC method
#include "memalloc.h"
#include "ode.h"
#include "graphlib.h"
#define pi 3.141592653589793
#define g 9.81e0                                // gravitational acceleration

double m, k;                          // mass of object, velocity coefficient
void Func(double t, double y[], double f[])         // RHSs of 1st order ODEs
{
   f[1] =  y[3];                  // y[1] = x, y[2] = y, y[3] = vx, y[4] = vy
   f[2] =  y[4];
   f[3] = -k/m * y[3]*fabs(y[3]);
   f[4] = -k/m * y[4]*fabs(y[4]) - g;
}

int main(int argc, wchar_t** argv)
{
   double *y, *tt, *xt, *yt;
   double Cd, ht, phi, rho, R, t, tmax, v0, vx0, vy0, x0, y0;
   int it, n, nt;

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
      if (y[2] < 0.e0) break;               // stop if object hits the ground
   }
   printf("xmax = %5.2f\n",y[1]);

   PyGraph w(argc, argv);
   w.GraphInit(1200,600);
   w.Plot(tt,yt,it,"blue",1,0.10,0.45,0.15,0.85,"t","y","Altitude");
   w.Plot(xt,yt,it,"blue",1,0.60,0.95,0.15,0.85,"x","y","Trajectory");
   w.MainLoop();
}

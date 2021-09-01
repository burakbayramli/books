// Oblique throw of an object with drag using the velocity Verlet method
#include "memalloc.h"
#include "ode.h"

#define g 9.81e0                                // gravitational acceleration

double m, k;                          // mass of object, velocity coefficient
void Forces(double m, double x, double y, double vx, double vy,
            double &fx, double &fy, double &Epot)
{
   fx = -k * vx*abs(vx);                                  // force components
   fy = -k * vy*abs(vy) - m*g;
   Epot = m*g*y;                                          // potential energy
}

int main()
{
   double ax, ay, Ekin, Epot, ht, t, tmax, vx0, vx, vy0, vy, x0, x, y0, y;
   FILE *out;

   m = 7e0;                                                 // mass of object
   k = 0.01e0;                                        // velocity coefficient
   x0 = 0e0; y0 = 3e0;                                    // initial position
   vx0 = 20e0; vy0 = 20e0;                                // initial velocity
   tmax = 20e0;                                                  // time span
   ht = 0.001e0;                                            // time step size

   out = fopen("throw.txt","w");                          // open output file
   fprintf(out,"      t         x         y        vx        vy\n");

   t = 0e0;
   x = x0; vx = vx0; ax = 0e0;                              // initial values
   y = y0; vy = vy0; ay = 0e0;
   fprintf(out,"%10.5f%10.5f%10.5f%10.5f%10.5f\n",t,x,y,vx,vy);

   while (t+ht <= tmax) {                                 // propagation loop
      Verlet2(ht,m,x,y,vx,vy,ax,ay,Ekin,Epot,Forces);
      t += ht;

      fprintf(out,"%10.5f%10.5f%10.5f%10.5f%10.5f\n",t,x,y,vx,vy);
      if (y < 0.e0) break;                  // stop if object hits the ground
   }
   fclose(out);
}

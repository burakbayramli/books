// Oblique throw of an object with drag using the Euler-Cromer method
#include "memalloc.h"
#include "ode.h"

#define g 9.81e0                                // gravitational acceleration

double m, k;                          // mass of object, velocity coefficient
void Func(double t, double y[], double dy[], double f[])      // RHSs of ODEs
{
   f[1] = -k/m * dy[1]*abs(dy[1]);
   f[2] = -k/m * dy[2]*abs(dy[2]) - g;
}

int main()
{
   double ht, t, tmax, vx0, vy0, x0, y0, *y, *dy;
   int n;
   FILE *out;

   m = 7e0;                                                 // mass of object
   k = 0.01e0;                                        // velocity coefficient
   x0 = 0e0; y0 = 3e0;                                    // initial position
   vx0 = 20e0; vy0 = 20e0;                                // initial velocity
   tmax = 20e0;                                                  // time span
   ht = 0.001e0;                                            // time step size

   n = 2;                                         // number of 2nd order ODEs
   y = Vector(1,n); dy = Vector(1,n);                  // solution components

   out = fopen("throw.txt","w");                          // open output file
   fprintf(out,"      t         x         y        vx        vy\n");

   t = 0e0;
   y[1] = x0; dy[1] = vx0;                                  // initial values
   y[2] = y0; dy[2] = vy0;
   fprintf(out,"%10.5f%10.5f%10.5f%10.5f%10.5f\n",t,y[1],y[2],dy[1],dy[2]);

   while (t+ht <= tmax) {                                 // propagation loop
      EulerCromer(t,ht,y,dy,n,Func);
      t += ht;

      fprintf(out,"%10.5f%10.5f%10.5f%10.5f%10.5f\n",t,y[1],y[2],dy[1],dy[2]);
      if (y[2] < 0.e0) break;               // stop if object hits the ground
   }

   fclose(out);
}

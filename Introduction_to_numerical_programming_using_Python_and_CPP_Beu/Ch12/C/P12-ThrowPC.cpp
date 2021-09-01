// Oblique throw of an object with drag using the Euler PC method
#include "memalloc.h"
#include "ode.h"

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
   double ht, t, tmax, vx0, vy0, x0, y0, *y;
   int n;
   FILE *out;

   m = 7e0;                                                 // mass of object
   k = 0.01e0;                                        // velocity coefficient
   x0 = 0e0; y0 = 3e0;                                    // initial position
   vx0 = 20e0; vy0 = 20e0;                                // initial velocity
   tmax = 20e0;                                                  // time span
   ht = 0.001e0;                                            // time step size

   n = 4;                                         // number of 1st order ODEs
   y = Vector(1,n);                                    // solution components

   out = fopen("throw.txt","w");                          // open output file
   fprintf(out,"      t         x         y        vx        vy\n");

   t = 0e0;
   y[1] = x0; y[3] = vx0;                                   // initial values
   y[2] = y0; y[4] = vy0;
   fprintf(out,"%10.5f%10.5f%10.5f%10.5f%10.5f\n",t,y[1],y[2],y[3],y[4]);

   while (t+ht <= tmax) {                                 // propagation loop
      EulerPC(t,ht,y,n,Func);
      t += ht;

      fprintf(out,"%10.5f%10.5f%10.5f%10.5f%10.5f\n",t,y[1],y[2],y[3],y[4]);
      if (y[2] < 0.e0) break;               // stop if object hits the ground
   }
   fclose(out);
}

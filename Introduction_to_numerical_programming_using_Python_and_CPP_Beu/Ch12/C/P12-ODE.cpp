// Solves a Cauchy problem for a 2nd order ODE by Euler's method
//    y" + y = 0,   y(0) = y0, y'(0) = y0'
// Equivalent problem: y[1] = y, y[2] = y'
//    y[1]' =  y[2],   y[1](0) = y0
//    y[2]' = -y[1],   y[2](0) = y0'
//---------------------------------------------------------------------------
#include "memalloc.h"
#include "ode.h"

void Func(double t, double y[], double f[])       // Right-hand sides of ODEs
{
   f[1] =  y[2];
   f[2] = -y[1];
}

int main()
{
   double ht, t, tmax, y0, dy0, *y;
   int n;
   FILE *out;

   y0 = 0e0; dy0 = 1e0;                    // initial values => y(t) = sin(t)
   tmax = 100e0;                                                 // time span
   ht = 0.05e0;                                                  // step size

   n = 2;                                         // number of 1st order ODEs
   y = Vector(1,n);                                    // solution components

   out = fopen("ode.txt","w");                            // open output file
   fprintf(out,"      t         y1        y2      check\n");

   t = 0e0;
   y[1] = y0; y[2] = dy0;                                   // initial values
   fprintf(out,"%10.5f%10.5f%10.5f%10.5f\n",
           t,y[1],y[2],y[1]*y[1]+y[2]*y[2]);

   while (t+ht <= tmax) {                                 // propagation loop
      Euler(t,ht,y,n,Func);
      t += ht;

      fprintf(out,"%10.5f%10.5f%10.5f%10.5f\n",
              t,y[1],y[2],y[1]*y[1]+y[2]*y[2]);
   }
   fclose(out);
}

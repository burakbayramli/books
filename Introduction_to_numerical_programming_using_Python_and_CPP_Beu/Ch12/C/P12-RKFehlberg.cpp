// Solves a Cauchy problem with adaptive step size control
#include "memalloc.h"
#include "ode.h"

void Func(double t, double y[], double f[])                   // RHSs of ODEs
{
   f[1] = y[2];
   f[2] = (t < 5e0 ? 1e0 : -100e0) * y[1];
}

int main()
{
   double eps, ht, ht1, t, tmax, y0, dy0, *y;
   int n;
   FILE *out;

   y0 = 0e0; dy0 = 1e0;                                     // initial values
   tmax = 10e0;                                                  // time span
   ht = 0.01e0;                                                  // step size
   eps = 1e-8;                                 // relative solution precision

   n = 2;                                         // number of 1st order ODEs
   y = Vector(1,n);                                    // solution components
 
   out = fopen("ode.txt","w");                            // open output file
   fprintf(out,"      t         y         h\n");

   t = 0e0;
   y[1] = y0; y[2] = dy0;                                   // initial values
   fprintf(out,"%10.5f%10.5f%10.5f\n",t,y[1],ht);

   ht1 = ht;                                       // initial step size guess
   while (t+ht < tmax) {                                  // propagation loop
      ht = ht1;                        // update initial step size with guess
      RKFehlberg(t,ht,ht1,eps,y,n,Func);
      t += ht;

      fprintf(out,"%10.5f%10.5f%10.5f\n",t,y[1],ht);
   }
   fclose(out);
}

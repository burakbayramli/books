// Angular motion of a nonlinear pendulum by the Euler-Cromer method
//    u" = -g/l * sin(u) - k * u',   u(0) = u0, u'(0) = u0'
#include "memalloc.h"
#include "ode.h"

#define pi 3.141592653589793
#define g 9.81e0                                // gravitational acceleration

double l, k;                         // pendulum length, velocity coefficient
double Func(double t, double u, double du)
{
   return -g/l * sin(u) - k * du;
}

int main()
{
   double ht, t, tmax, u0, u, du0, du;
   FILE *out;

   l = 1e0;                                                // pendulum length
   k = 0e0;                                           // velocity coefficient
   u0 = 0.5e0*pi;                                     // initial displacement
   du0 = 0e0;                                           // initial derivative
   tmax = 20e0;                                                  // time span
   ht = 0.01e0;                                             // time step size

   out = fopen("pendulum.txt","w");                       // open output file
   fprintf(out,"      t         u        du\n");

   t = 0e0;
   u = u0; du = du0;
   fprintf(out,"%10.5f%10.5f%10.5f\n",t,u,du);

   while (t+ht <= tmax) {                                 // propagation loop
      EulerCromer1(t,ht,u,du,Func);
      t += ht;

      fprintf(out,"%10.5f%10.5f%10.5f\n",t,u,du);
   }
   fclose(out);
}

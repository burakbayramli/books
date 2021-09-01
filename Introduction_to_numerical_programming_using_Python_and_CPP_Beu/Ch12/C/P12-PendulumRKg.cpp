// Angular motion of a nonlinear pendulum by the Runge-Kutta method
//    u" = -g/l * sin(u) - k * u',   u(0) = u0, u'(0) = u0'
#include "memalloc.h"
#include "ode.h"
#include "integral.h"
#include "graphlib.h"

#define pi 3.141592653589793
#define g 9.81e0                                // gravitational acceleration

double l, k;                         // pendulum length, velocity coefficient
void Func(double t, double u[], double f[])          // RHS of 1st order ODEs
{
   f[1] =  u[2];                                       // u[1] = u, u[2] = u'
   f[2] = -g/l * sin(u[1]) - k * u[2];
}

double mKel;
double fKel(double z) { return 1e0 / sqrt((1e0-z*z)*(1e0-mKel*z*z)); }
//===========================================================================
double Kel(double m)
//---------------------------------------------------------------------------
// Returns the complete elliptic integral of the 1st kind
// Uses integrand fKel, passing parameter m by the global variable mKel
// Calls: qImprop2 (integral.h)
//---------------------------------------------------------------------------
{
   const double eps = 1e-7;                             // relative precision
   mKel = m;
   return qImprop2(fKel,0e0,1e0,eps);
}

int main(int argc, wchar_t** argv)
{
   double ht, t, t1, t2, tmax, T0, T, Tex, u0, du0, us, *u, *tt, *ut, *vt;
   int it, n, nt, nT;

   l = 1e0;                                                // pendulum length
   k = 0e0;                                           // velocity coefficient
   u0 = 0.5e0*pi;                                     // initial displacement
   du0 = 0e0;                                           // initial derivative
   tmax = 20e0;                                                  // time span
   ht = 0.001e0;                                            // time step size

   n = 2;                                         // number of 1st order ODEs
   nt = int(tmax/ht + 0.5) + 1;                       // number of time steps
   u = Vector(1,n);                                    // solution components
   tt = Vector(1,nt); ut = Vector(1,nt); vt = Vector(1,nt);   // for plotting

   t = 0e0; it = 1;
   u[1] = u0; u[2] = du0;                                   // initial values
   tt[1] = t; ut[1] = u[1]; vt[1] = u[2];               // store for plotting

   nT = 0;                                          // number of half-periods
   t1 = t2 = 0e0;                                  // bounding solution zeros
   us = u[1];                                                // save solution
   while (t+ht <= tmax) {                                 // propagation loop
      RungeKutta(t,ht,u,n,Func);
      t += ht; it += 1;

      if (u[1]*us < 0e0) {            // count solution passages through zero
         if (t1 == 0e0) { t1 = t; }                           // initial zero
         else { t2 = t; nT += 1; }                              // final zero
      }
      us = u[1];                                             // save solution

      tt[it] = t; ut[it] = u[1]; vt[it] = u[2];         // store for plotting
   }

   T = 2e0*(t2-t1) / nT;                                 // calculated period
   T0 = 2e0*pi*sqrt(l/g);                                  // harmonic period
   Tex = 2/pi * T0 * Kel(pow(sin(0.5e0*u0),2));               // exact period
   printf("u0 = %7.5f  T/T0 = %7.5f + (%8.1e)",u0,T/T0,(Tex-T)/T0);

   PyGraph w(argc, argv);
   w.GraphInit(1200,600);

   w.Plot(tt,ut,it,"blue",1,0.10,0.45,0.15,0.85,
          "t (s)","u (rad)","Displacement of nonlinear pendulum");
   w.Plot(ut,vt,it,"blue",1,0.60,0.95,0.15,0.85,
          "u (rad)","u' (rad/s)","Trajectory of pendulum in phase space");

   w.MainLoop();
}

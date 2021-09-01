// Solves a Cauchy problem with adaptive step size control
#include "memalloc.h"
#include "ode.h"
#include "graphlib.h"

void Func(double t, double y[], double f[])                   // RHSs of ODEs
{
   f[1] = y[2];
   f[2] = (t < 5e0 ? 1e0 : -100e0) * y[1];
}

int main(int argc, wchar_t** argv)
{
   double eps, ht, ht1, t, tmax, y0, dy0, *y, *tp, *yp, *hp;
   int it, n, nt;

   y0 = 0e0; dy0 = 1e0;                                     // initial values
   tmax = 10e0;                                                  // time span
   ht = 0.01e0;                                                  // step size
   eps = 1e-8;                                 // relative solution precision

   n = 2;                                         // number of 1st order ODEs
   nt = int(tmax/0.001 + 0.5) + 1;                    // number of time steps
   y = Vector(1,n);                                    // solution components
   tp = Vector(1,nt); yp = Vector(1,nt); hp = Vector(1,nt);// for t-dep plots

   t = 0e0; it = 1;
   y[1] = y0; y[2] = dy0;                                   // initial values
   tp[1] = t; yp[1] = y[1]; hp[1] = ht;                 // store for plotting

   ht1 = ht;                                       // initial step size guess
   while (t+ht < tmax) {                                  // propagation loop
      ht = ht1;                        // update initial step size with guess
      RKFehlberg(t,ht,ht1,eps,y,n,Func);
      t += ht; it += 1;
                                                        // store for plotting
      if (it <= nt) { tp[it] = t; yp[it] = y[1]; hp[it] = ht; }
   }

   PyGraph w(argc, argv);
   w.GraphInit(1200,600);

   w.Plot(tp,yp,it,"blue",1,0.10,0.45,0.15,0.85,"t","y","Solution");
   w.Plot(tp,hp,it,"blue",1,0.60,0.95,0.15,0.85,"t","h","Step size");

   w.MainLoop();
}

// Solution of Kepler's equation
#include <math.h>
#include "memalloc.h"
#include "roots.h"
#include "graphlib.h"
#define pi 3.141592653589793
#define pi2 2e0*pi

double e, M;                  // global variables: eccentricity, mean anomaly
double Func(double E, double &df) {
   df = 1e0 - e * cos(E);
   return E - e * sin(E) - M; }

int main(int argc, wchar_t** argv)
{
   int i, ierr, n = 101;
   double df, E, Emax, h, t, t0, T, *x, *y;

   x = Vector(1,n); y = Vector(1,n);
   
   PyGraph w(argc, argv);
   w.GraphInit(1200,600);
                                             // Halley comet (Mottmann 1986):
   e  = 0.9672671e0;                                          // eccentricity
   T  = 75.960000e0;                                                // period
   t0 = 1986.1113e0;                                    // time at perihelion

//---------------------------------------------------------------------------
   t = 1986.2491e0;
   M = pi2 / T * (t - t0);                                    // mean anomaly
   Emax = 1e0;                     // eccentric anomaly vs. Kepler's function
   h = Emax / (n-1);
   for (i=1; i<=n; i++) {
      E = (i-1)*h;
      x[i] = E; y[i] = Func(E,df);
   }
   w.Plot(x,y,n,"blue",1,0.10,0.45,0.15,0.85,
          "E (rad)","f(E)","Kepler's function");

   ierr = Newton(Func,0e0,Emax,E);                 // solve Kepler's equation
   printf("E = %6.4f rad at t = %8.4f years\n",E,t);

//---------------------------------------------------------------------------
   h = T / (n-1);                     // time dependence of eccentric anomaly
   x[1] = t0; y[1] = E = 0e0;
   for (i=2; i<=n; i++) {
      t = t0 + (i-1)*h;
      M = pi2 / T * (t - t0);                                 // mean anomaly
      ierr = Newton(Func,0e0,pi2,E);               // solve Kepler's equation
      x[i] = t; y[i] = E;
   }
   w.Plot(x,y,n,"red",1,0.60,0.95,0.15,0.85,
          "t (years)","E (rad)","Eccentric anomaly");

   w.MainLoop();
}

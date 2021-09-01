// Relative motion of the Earth and Moon using the velocity Verlet method
#include "memalloc.h"
#include "ode.h"
#include "coords.h"
#include "graphlib.h"

double G = 6.67384e-11;                // gravitational constant m^3 / kg s^2

//===========================================================================
void Forces(double m[], double x[], double y[], double z[],
            double fx[], double fy[], double fz[], int n, double &Epot)
//---------------------------------------------------------------------------
// Returns gravitational forces acting on a system of n point-masses
//---------------------------------------------------------------------------
{
   double dx, dy, dz, fr, r, r2;
   int i, j;

   Epot = 0e0;
   for (i=1; i<=n; i++) { fx[i] = fy[i] = fz[i] = 0e0; }

   for (i=1; i<=n-1; i++)                              // loop over all pairs
      for (j=i+1; j<=n; j++) {
         dx = x[i] - x[j];                 // components of relative distance
         dy = y[i] - y[j];
         dz = z[i] - z[j];
         r2 = dx*dx + dy*dy + dz*dz;                      // squared distance
         r = sqrt(r2);
         fr = G * m[i] * m[j] / r;                             // |force| * r

         Epot += fr;                                // total potential energy

         fr /= r2;                                             // |force| / r
         fx[i] -= fr * dx; fx[j] += fr * dx;        // total force components
         fy[i] -= fr * dy; fy[j] += fr * dy;
         fz[i] -= fr * dz; fz[j] += fr * dz;
      }
}

int main(int argc, wchar_t** argv)
{
   double *m, *x, *y, *z, *vx, *vy, *vz, *ax, *ay, *az, Ekin, Epot;
   double *tp, *dp, *xp, *yp;
   double day, d0, ht, km, mEarth, mMoon, month, t, tmax, v0;
   int ip, it, n, n1, np, nt;

   mEarth = 5.97e24;                                     // Earth's mass (kg)
   mMoon  = 7.35e22;                                      // Moon's mass (kg)
   d0 = 4.06e8;                          // Earth-Moon distance at apogee (m)
   v0 = 969e0;                             // initial relative velocity (m/s)
   km = 1e3;
   month = 27.32;             // sidereal month: Moon's orbital period (days)
   day = 3600 * 24;                                         // day length (s)
   tmax = 2 * month * day;                   // time span: 2 lunar months (s)
   ht = 1e0;                                                 // time step (s)

   n = 2;                                                // number of planets
   m = Vector(1,n);                                          // planet masses
   x = Vector(1,n); vx = Vector(1,n); ax = Vector(1,n);        // coordinates
   y = Vector(1,n); vy = Vector(1,n); ay = Vector(1,n);         // velocities
   z = Vector(1,n); vz = Vector(1,n); az = Vector(1,n);      // accelerations

   nt = int(tmax/ht + 0.5) + 1;                       // number of time steps
   n1 = 10000;                             // number of steps between records
   np = int(float(nt-1)/n1) + 1;                  // number of plotted points
   nt = (np-1) * n1 + 1;                    // corrected number of time steps
   tp = Vector(1,np); dp = Vector(1,np);                   // plotting arrays
   xp = Vector(1,np); yp = Vector(1,np);

   m[1] = mEarth;                                    // initial configuration
   x[1] = 0e0; vx[1] = 0e0; ax[1] = 0e0;
   y[1] = 0e0; vy[1] = 0e0; ay[1] = 0e0;
   z[1] = 0e0; vz[1] = 0e0; az[1] = 0e0;
   m[2] = mMoon;
   x[2] = d0 ; vx[2] = 0e0; ax[2] = 0e0;
   y[2] = 0e0; vy[2] = v0 ; ay[2] = 0e0;
   z[2] = 0e0; vz[2] = 0e0; az[2] = 0e0;

   MovetoCM(m,x,y,z,n);                                  // move to CM system
   MovetoCM(m,vx,vy,vz,n);                           // cancel total momentum

   t = 0e0;                                                 // initialization
   it = ip = 1;
   tp[ip] = t / day;
   dp[ip] = sqrt(pow(x[1]-x[2],2)+pow(y[1]-y[2],2)) / km; // Earth-Moon dist.
   xp[ip] = x[2] / km; yp[ip] = y[2] / km;                 // Moon's position

   PyGraph w(argc, argv);
   w.GraphInit(1200,600);

   while (t+ht <= tmax) {                                 // propagation loop
      t += ht; it += 1;
      Verlet(ht,m,x,y,z,vx,vy,vz,ax,ay,az,n,Ekin,Epot,Forces);

      if (it % n1 == 0) {
         ip += 1;
         tp[ip] = t / day;
         dp[ip] = sqrt(pow(x[1]-x[2],2)+pow(y[1]-y[2],2)) / km;
         xp[ip] = x[2] / km; yp[ip] = y[2] / km;

         w.GraphClear();
         w.Plot(tp,dp,ip,"blue",1,0.12,0.47,0.15,0.85,
                "t (days)","d (km)","Earth-Moon distance");
         w.Plot(xp,yp,ip,"red",2,0.62,0.97,0.15,0.85,
                "x (km)","y (km)","Moon's trajectory in the CM system");
         w.GraphUpdate();
      }
   }

   w.MainLoop();
}

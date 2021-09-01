// Monte Carlo calculation of the mass center of a torus of radii R and r,
// centered at the origin and with Oz as symmetry axis
#include <stdio.h>
#include <math.h>
#include "random.h"

double R, r;
double Func(double x, double y, double z)
{
   double dR, dr, Rp, rp;
   Rp = sqrt(x*x + y*y);                             // major radial distance
   dR = R - Rp;
   rp = sqrt(dR*dR + z*z);                           // minor radial distance
   dr = 1e0 - rp/r;
   return (rp <= r ? dr*dr : 0e0);                            // zero-padding
}

int main()
{
   double dens, Lx, Ly, Lz, V, x, y, z;
   double f, sm, sm2, sx, sx2, sy, sy2, sz, sz2;
   double m, sigm, sigx, sigy, sigz, xc, yc, zc;
   long i, n;

   R = 3e0; r = 1e0;                             // major & minor torus radii
   Lx = Ly = R + r; Lz = r;                    // extended domain: 1st octant
   V = 8e0 * Lx * Ly * Lz;                 // volume of total extended domain

   n = 10000000;                                 // number of sampling points

   seed();

   sm  = sx  = sy  = sz  = 0e0;
   sm2 = sx2 = sy2 = sz2 = 0e0;
   for (i=1; i<=n; i++) {
      x = Lx * (2e0*random() - 1e0);                        // -Lx <= x <= Lx
      y = Ly * (2e0*random() - 1e0);                        // -Ly <= y <= Ly
      z = Lz * (2e0*random() - 1e0);                        // -Lz <= x <= Lz
      dens = Func(x,y,z);                                          // density
      if (dens) {
         f = dens    ; sm += f; sm2 += f * f;                         // sums
         f = dens * x; sx += f; sx2 += f * f;
         f = dens * y; sy += f; sy2 += f * f;
         f = dens * z; sz += f; sz2 += f * f;
      }
   }
   sm /= n; sx /= n; sy /= n; sz /= n;                            // averages
   m  = V * sm; sigm = V * sqrt((sm2/n - sm*sm)/n); f = V/m;     // integrals
   xc = f * sx; sigx = f * sqrt((sx2/n - sx*sx)/n);
   yc = f * sy; sigy = f * sqrt((sy2/n - sy*sy)/n);
   zc = f * sz; sigz = f * sqrt((sz2/n - sz*sz)/n);

   printf("m  = %8.5f +/- %8.5f\n",m ,sigm);
   printf("xc = %8.5f +/- %8.5f\n",xc,sigx);
   printf("yc = %8.5f +/- %8.5f\n",yc,sigy);
   printf("zc = %8.5f +/- %8.5f\n",zc,sigz);
}

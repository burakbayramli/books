// Evaluates a 3D integral on a torus using Cartesian coordinates
#include <stdio.h>
#include <math.h>
#include "integral.h"

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
   double ax, ay, az, bx, by, bz, I;
   int nx, ny, nz;

   R = 3e0; r = 1e0;                             // major & minor torus radii
   ax = ay = az = 0e0;                            // bounding box: 1st octant
   bx = by = R+r; bz = r;                          // multiply results with 8
   nx = ny = 200;                                    // equal density of mesh
   nz = int(nx * r/(R+r));                            // points along x, y, z

   I = 8e0 * qSimpson3D(Func,ax,bx,nx,ay,by,ny,az,bz,nz);
   printf("I Simpson  = %f\n",I); 
   I = 8e0 * qGaussLeg3D(Func,ax,bx,nx,ay,by,ny,az,bz,nz);
   printf("I GaussLeg = %f\n",I);                       // result 9.869603...
}

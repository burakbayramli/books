// Evaluates a 3D integral using cylindrical coordinates
#include <stdio.h>
#include <math.h>
#include "integral.h"

#define pi 3.141592653589793

double Func(double r, double phi, double z)
{
   return exp(-r) * pow(cos(phi),2) * z;
}

int main()
{
   double a, az, bz, I;
   int nr, np, nz;

   a = 35e0;                                                  // radial limit
   az = 0e0; bz = 1e0;                                        // axial limits
   nr = 150; np = 20; nz = 2;                       // numbers of mesh points

   I = qSimpsonCyl(Func,a,az,bz,nr,np,nz);
   printf("I SimpsonCyl = %f\n",I); 
   printf("I exact      = %f\n",pi/2);
}

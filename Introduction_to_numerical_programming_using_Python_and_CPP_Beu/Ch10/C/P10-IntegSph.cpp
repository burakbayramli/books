// Evaluates a 3D integral using spherical coordinates
#include <stdio.h>
#include <math.h>
#include "integral.h"

#define pi 3.141592653589793

double Func(double r, double theta, double phi)
{                                         // probability density for 2p state
   const double c = 1e0/(4e0*sqrt(2e0*pi));
   double psi;

   psi = c * r * exp(-0.5e0*r) * cos(theta);
   return psi * psi;
}

int main()
{
   double a, I;
   int nr, nt, np;

   a = 35e0;                                                  // radial limit
   nr = 150; nt = 60; np = 3;
   I = qSimpsonSph(Func,a,nr,nt,np);
   printf("I SimpsonSph = %10.8f\n",I); 

   nr = 3; nt = 9; np = 1;
   I = qGaussSph(Func,nr,nt,np);
   printf("I GaussSph   = %10.8f\n",I); 
}

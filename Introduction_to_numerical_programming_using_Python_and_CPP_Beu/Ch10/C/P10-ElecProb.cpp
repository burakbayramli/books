// Total localization probability of an electron in the H atom
#include <stdio.h>
#include <math.h>
#include "integral.h"

#define pi 3.141592653589793

double Func(double r, double theta, double phi)
{                                       // probability density for 3d_0 state
   const double c = 1e0/(81e0*sqrt(6e0*pi));
   double cost, psi;

   cost = cos(theta);
   psi = c * r * r * exp(-r/3e0) * (3e0*cost*cost - 1e0);
   return psi * psi;
}

int main()
{
   double a, I;
   int nr, nt, np;

   a = 40e0;                                                  // radial limit
   nr = 100; nt = 70; np = 3;
   I = qSimpsonSph(Func,a,nr,nt,np);
   printf("I SimpsonSph = %10.8f\n",I); 

   nr = 10; nt = 10; np = 1;
   I = qGaussSph(Func,nr,nt,np);
   printf("I GaussSph   = %10.8f\n",I); 
}

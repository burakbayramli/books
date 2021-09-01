// Norm of associated Legendre functions and spherical harmonics
#include <stdio.h>
#include <math.h>
#include "specfunc.h"
#include "integral.h"

int l, m;

double aLegendre2(double x)          // squared normalized Legendre functions
{
   double f;
   int i;
   
   f = 1e0;
   for (i=l-m+1; i<=l+m; i++) f *= i;                    // (l+|m|)!/(l-|m|)!
   f = sqrt((2*l+1)/(2e0*f)) * aLegendre(l,m,x);             // normalization
   return f * f;
}

double SpherY2(double theta, double phi)       // squared spherical harmonics
{
   double ReY, ImY;
   
   SpherY(l,m,theta,phi,ReY,ImY);
   return ReY * ReY + ImY * ImY;
}

int main()
{
   double NormL, NormY;
   int lmax, nt, np;

   lmax = 5;                                               // maximum order l
   nt = 181; np = 3;   //number of mesh points for angular Simpson quadrature

   for (l=0; l<=lmax; l++) {
      for (m=0; m<=l; m++) {
         NormL = qRomberg(aLegendre2,-1e0,1e0,1e-10);
         NormY = qSimpsonAng(SpherY2,nt,np);
         printf("l = %i m = %i NormL = %13.10f NormY = %13.10f\n",
                l,m,NormL,NormY);
      }
   }
}

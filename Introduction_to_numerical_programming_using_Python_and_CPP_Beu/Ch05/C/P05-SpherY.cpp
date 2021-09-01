// Checks the addition theorem for spherical harmonics
#include <stdio.h>
#include <math.h>
#include "specfunc.h"

#define pi 3.141592653589793

int main()
{
   double cosgam, phi1, phi2, theta1, theta2;
   double d, P, ImY1, ImY2, ReY1, ReY2, sumIm, sumRe;
   int l, m;

   l = 5;
   theta1 = pi/5; phi1 = pi/9;
   theta2 = pi/3; phi2 = pi/8;
   
   cosgam = cos(theta1) * cos(theta2) \
          + sin(theta1) * sin(theta2) * cos(phi2 - phi1);
   P = Legendre(l,cosgam,d);
   
   sumRe = 0e0;
   sumIm = 0e0;
   for (m=-l; m<=l; m++) {
       SpherY(l, m, theta1, phi1, ReY1, ImY1);
       SpherY(l, m, theta2, phi2, ReY2, ImY2);
   
       sumRe += ReY2 * ReY1 + ImY2 * ImY1;
       sumIm += ReY2 * ImY1 - ImY2 * ReY1;
   }
   sumRe *= 4*pi/(2*l+1);
   sumIm *= 4*pi/(2*l+1);
   
   printf("%f  %f\n",P-sumRe,sumIm);              // P - sumRe = 0, sumIm ~ 0
}

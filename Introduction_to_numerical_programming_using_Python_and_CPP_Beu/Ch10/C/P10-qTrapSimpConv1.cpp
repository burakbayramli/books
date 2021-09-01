// Convergence of trapezoidal and Simpson quadratures
#include <stdio.h>
#include <math.h>
#include "integral.h"
#include "specfunc.h"

int nord;                                     // order of Legendre polynomial

double Func(double x)               // integrand: norm of Legendre polynomial
{
    double d, f;
    f = Legendre(nord,x,d);
    return f * f;
}

int main(){
   double a, b, errS, errS0,  errT, errT0, facS, facT, I, IS, IT;
   int i, n;

   nord = 4;                                  // order of Legendre polynomial
   a = -1e0; b = 1e0;                                   // integration domain

   I = 2e0/(2e0*nord+1e0);                                    // exact result
   printf("Exact result = %f",I);
   printf("\n   n       IT        errT   facT      IS        errS   facS\n");

   n = 1;
   errT0 = 1e0; errS0 = 1e0;
   for (i=1; i<=10; i++) {
      n = 2*n;                                         // number of intervals

      IT = qTrapz(Func,a,b,n+1);                          // trapezoidal rule
      errT = fabs(1e0 - IT/I);                              // relative error
      facT = errT0/errT;                                   // error reduction

      IS = qSimpson(Func,a,b,n+1);                          // Simpson's rule
      errS = fabs(1e0 - IS/I);                              // relative error
      facS = errS0/errS;                                   // error reduction

      printf("%5d%11.6f%10.1e%6.1f%11.6f%10.1e%6.1f\n",
             n,IT,errT,facT,IS,errS,facS);

      errT0 = errT; errS0 = errS;
   }
}

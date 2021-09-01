// Convergence of trapezoidal and Simpson quadratures
#include <stdio.h>
#include <math.h>
#include "integral.h"
#include "specfunc.h"

double Func(double x) { return sin(x); }                         // integrand

int main(){
   double a, b, errS, errS0,  errT, errT0, facS, facT, I, IS, IT;
   int i, n;

   a = 0e0; b = pi;                                     // integration domain

   I = cos(a) - cos(b);                                       // exact result
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

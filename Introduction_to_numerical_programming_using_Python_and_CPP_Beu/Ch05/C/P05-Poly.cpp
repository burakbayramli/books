// Evaluates a polynomial, its derivative and quotient
#include <stdio.h>
#include "elemfunc.h"

int main()
{
   const int n = 3;                                   // degree of polynomial
   double a[] = {6e0, -11e0, 6e0, 3e0};                       // coefficients
   double b[n+1];                                               // work array
   double P, Q, R, x0, x1;
   int i;

   x0 = 1e0;

   printf("Coefficients of polynomial:\n");
   for (i=0; i<=n; i++) printf("%10.2f  ",a[i]); printf("\n");

   printf("\nPolynomial value:\n");               // evaluation of polynomial
   printf("P(%0.2f) = %0.2f\n",x0,Poly(x0, a, n));

   PolyDerive(a, b, n);                           // derivative of polynomial
   printf("\nCoefficients of derivative:\n");
   for (i=0; i<=n; i++) printf("%10.2f  ",b[i]); printf("\n");

   PolyDivide(x0, a, b, n);                   // synthetic division by (x-x0)
   printf("\nCoefficients of quotient and remainder:\n");
   for (i=0; i<=n; i++) printf("%10.2f  ",b[i]); printf("\n");
   R = b[n];                                                     // remainder
   printf("\nCheck of remainder:\n");
   printf("R - P(x0) = %0.2f\n",R-Poly(x0,a,n));

   x1 = 2e0;                               // check synthetic division for x1
   P = Poly(x1,a,n);                                      // polynomial value
   Q = Poly(x1,b,n-1);                                      // quotient value
   printf("\nCheck of division for x1 = %0.2f :\n",x1);
   printf("P(x1) - (x1-x0)Q(x1) - R = %0.2f\n",P - (x1-x0)*Q - R);
}

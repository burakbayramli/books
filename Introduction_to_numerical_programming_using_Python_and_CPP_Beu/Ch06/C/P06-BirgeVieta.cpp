// Real roots of a polynomial
#include <stdio.h>
#include "roots.h"

int main()
{
   const int n = 4;                                   // degree of polynomial
   double ax[] = {1e0, 0e0, -5e0, 0e0, 4e0};                  // coefficients
   double xx[n+1];                                                   // zeros
   int i, nx;

   BirgeVieta(ax,n,xx,nx);                                  // nx zeros found
   
   for (i=1; i<=nx; i++) printf("x[%1i] = %10.5f\n",i,xx[i]);
   if (nx <= 0) printf(" Non-existing real zeros\n");
}

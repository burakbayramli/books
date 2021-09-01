// Solves system with tridiagonal matrix by LU factorization
#include <stdio.h>
#include "memalloc.h"
#include "linsys.h"

int main()
{
   double *a, *b, *c, *d;
   int i, n;

   n = 4;                                                  // order of system
   a = Vector(1,n);                                         // lower diagonal
   b = Vector(1,n);                                          // main diagonal
   c = Vector(1,n);                                         // upper diagonal
   d = Vector(1,n);                            // constant terms and solution

   a[1] = 0; b[1] = 1; c[1] = 2; d[1] = 1;
   a[2] = 2; b[2] = 1; c[2] = 2; d[2] = 2;
   a[3] = 2; b[3] = 1; c[3] = 2; d[3] = 3;
   a[4] = 2; b[4] = 1; c[4] = 0; d[4] = 4;  // Solution: -3.0, 2.0, 3.0, -2.0

   TriDiagSys(a,b,c,d,n);                         // solve tridiagonal system

   printf("Solution:\n");
   for (i=1; i<=n; i++) printf("%10.3f",d[i]);
   printf("\n");
}

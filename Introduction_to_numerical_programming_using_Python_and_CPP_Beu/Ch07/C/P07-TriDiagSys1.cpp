// Solves system with tridiagonal matrix by LU factorization
#include <stdio.h>
#include "memalloc.h"
#include "linsys.h"

int main()
{
   double *a, *a0, *b, *b0, *c, *c0, *d, *x, err, erri;
   int i, n;

   n = 100;                                                // order of system
   a = Vector(1,n); a0 = Vector(1,n);                       // lower diagonal
   b = Vector(1,n); b0 = Vector(1,n);                        // main diagonal
   c = Vector(1,n); c0 = Vector(1,n);                       // upper diagonal
   d = Vector(1,n); x  = Vector(1,n);          // constant terms and solution

   for (i=1; i<=n; i++) {                                  // generate system
      if (i > 1) a[i] = a0[i] = rand();                        // a[i], i=2,n
      if (i < n) c[i] = c0[i] = rand();                      // c[i], i=1,n-1
      b[i] = b0[i] = rand();
      d[i] = x[i]  = rand();
   }

   TriDiagSys(a,b,c,x,n);                         // solve tridiagonal system

   err = 0e0;                                          // check max(Ax-d) = 0
   for (i=1; i<=n; i++) {
      erri = b0[i]*x[i] - d[i];                        // element i of (Ax-d)
      if (i > 1) erri += a0[i]*x[i-1];
      if (i < n) erri += c0[i]*x[i+1];
      err = max(err,fabs(erri));
   }
   printf("max(Ax-d) = %.1e\n",err);
}

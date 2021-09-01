// Solves multiple linear systems by LU factorization
#include <stdio.h>
#include "memalloc.h"
#include "linsys.h"
#include "matutil.h"

int main()
{
   double **a, **a0, *b, *x, det, err, erri;
   int *ipivot, i, j, k, n;

   n = 5;                                                  // order of system
   a  = Matrix(1,n,1,n);                                     // system matrix
   a0 = Matrix(1,n,1,n);                                     // backup matrix
   b = Vector(1,n);                                         // constant terms
   x = Vector(1,n);                                               // solution
   ipivot = IVector(1,n);                                           // pivots

   for (i=1; i<=n; i++)                                      // random matrix
      for (j=1; j<=n; j++) a[i][j] = a0[i][j] = rand();
   printf("A:\n");
   MatPrint(a,n,n);

   LUFactor(a,ipivot,n,det);                         // LU decomposition of a
   printf("LU decomposition:\n");
   MatPrint(a,n,n);

   for (k=1; k<=n; k++) {                       // loop over constant vectors
      for (i=1; i<=n; i++) b[i] = x[i] = rand();     // random constant terms
      printf("b:\n");
      VecPrint(b,n);

      LUSystem(a,ipivot,x,n);                          // solve linear system

      err = 0e0;                                       // check max(Ax-b) = 0
      for (i=1; i<=n; i++) {
         erri = -b[i];
         for (j=1; j<=n; j++) erri += a0[i][j] * x[j];  // element i of (Ax-b)
         err = max(err,fabs(erri));
      }
      printf("   max(Ax-b) = %.1e\n",err);
   }
}

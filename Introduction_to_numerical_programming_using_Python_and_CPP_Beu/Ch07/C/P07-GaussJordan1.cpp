// Solves matrix equation by the Gauss-Jordan method
#include <stdio.h>
#include "memalloc.h"
#include "linsys.h"
#include "matutil.h"

int main()
{
   double **a, **b, **c, **d, det;
   int i, j, n;

   n = 5;                                                  // order of system
   a = Matrix(1,n,1,n);                                      // system matrix
   b = Matrix(1,n,1,n);                        // constant terms and solution
   c = Matrix(1,n,1,n);                              // copy of system matrix
   d = Matrix(1,n,1,n);                                         // work array

   for (i=1; i<=n; i++)                                      // random matrix
      for (j=1; j<=n; j++) a[i][j] = rand();
   printf("A:\n");
   MatPrint(a,n,n);

   for (i=1; i<=n; i++) {                                      // unit matrix
      for (j=1; j<=n; j++) b[i][j] = 0e0;
      b[i][i] = 1e0;
   }
   printf("B:\n");
   MatPrint(b,n,n);

   MatCopy(a,c,n,n);                                    // save system matrix

   GaussJordan(a,b,n,n,det);                    // solve system, inverse in a

   printf("Check A^(-1) - X = 0:\n");
   MatDiff(a,b,d,n,n);             // difference between inverse and solution
   MatPrint(d,n,n);

   printf("Check A^(-1)A = I:\n");
   MatProd(a,c,d,n,n,n);                    // multiply inverse with original
   MatPrint(d,n,n);
}

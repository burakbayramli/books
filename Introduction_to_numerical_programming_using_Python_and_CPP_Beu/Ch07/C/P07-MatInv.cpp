// Check of matrix inversion using LU decomposition
#include <stdio.h>
#include "memalloc.h"
#include "linsys.h"
#include "matutil.h"

int main()
{
   double **a, **b, **c, det;
   int i, j, n;

   n = 5;                                                  // order of matrix
   a = Matrix(1,n,1,n);                        // original matrix and inverse
   b = Matrix(1,n,1,n);                                 // backup of original
   c = Matrix(1,n,1,n);                                       // check matrix

   for (i=1; i<=n; i++)                             // generate random matrix
      for (j=1; j<=n; j++) a[i][j] = rand();

   printf("Original matrix:\n");
   MatPrint(a,n,n);

   MatCopy(a,b,n,n);                                // backup original matrix

   MatInv(a,n,det);                                        // invert original
   if (det == 0e0) { printf("Singular matrix\n"); exit(1); }

   printf("Inverse matrix:\n");
   MatPrint(a,n,n);

   printf("Check A^(-1)A = I:\n");
   MatProd(a,b,c,n,n,n);                    // multiply inverse with original
   MatPrint(c,n,n);                                     // print check matrix
}

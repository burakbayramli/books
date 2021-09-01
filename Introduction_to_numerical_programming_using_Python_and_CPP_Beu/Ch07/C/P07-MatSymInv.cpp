// Inversion of symmetric positive-definite matrix by Cholesky factorization
#include <stdio.h>
#include "memalloc.h"
#include "linsys.h"
#include "matutil.h"

int main()
{
   double **a, **b, **c, det, err;
   int i, j, n;

   n = 100;                                             // order of matrices
   a = Matrix(1,n,1,n);
   b = Matrix(1,n,1,n);
   c = Matrix(1,n,1,n);

   for (i=1; i<=n; i++) {          // generate random lower triangular matrix
      for (j=1  ; j<=i; j++) a[i][j] = (double)rand()/RAND_MAX;
      for (j=i+1; j<=n; j++) a[i][j] = 0e0;                 // upper triangle
      a[i][i] += 1e0;                                    // increase diagonal
   }

   MatCopy(a,b,n,n);
   MatTrans(b,n);                   // create upper triangular matrix b = a^T
   MatProd(a,b,c,n,n,n);             // c = a a^T symmetric positive-definite
   MatCopy(c,a,n,n);
   MatCopy(c,b,n,n);

   MatSymInv(a,n,det);                   // inverse by Cholesky factorization

   MatProd(a,b,c,n,n,n);                        // c = a^(-1)a  "unit" matrix
   printf("A^(-1)A (sample):\n\n");
   MatPrint(c,5,5);                          // print sample of "unit" matrix

   for (i=1; i<=n; i++) c[i][i] -= 1e0;         // transform to "zero" matrix
   err = MatNorm(c,n,n);                                 // max(abs(c[i][j]))
   printf("\nMaximum error: %9.2e\n",err);
}

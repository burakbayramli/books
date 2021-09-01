// Checks identity (A B)_trans = B_trans A_trans for random matrices A and B
#include <stdio.h>
#include "memalloc.h"
#include "matutil.h"

int main()
{
   double **A, **B, **C, **D;
   int i, j, n;

   printf("n = "); scanf("%i",&n);

   A = Matrix(1,n,1,n); B = Matrix(1,n,1,n);              // allocates arrays
   C = Matrix(1,n,1,n); D = Matrix(1,n,1,n);

   for (i=1; i<=n; i++)               // array A: random sub-unitary elements
      for (j=1; j<=n; j++) A[i][j] = rand()/(RAND_MAX+1e0);
   printf("Array A:\n");
   MatPrint(A,n,n);

   for (i=1; i<=n; i++)               // array B: random sub-unitary elements
      for (j=1; j<=n; j++) B[i][j] = rand()/(RAND_MAX+1e0);
   printf("Array B:\n");
   MatPrint(B,n,n);

   MatProd(A,B,C,n,n,n);                                               // A*B
   MatTrans(C,n);                                              // (A*B)_trans

   MatTrans(A,n);                                                  // A_trans
   MatTrans(B,n);                                                  // B_trans
   MatProd(B,A,D,n,n,n);                                  // B_trans* A_trans

   MatDiff(C,D,D,n,n);                     // (A*B)_trans - B_trans * A_trans
   printf("Norm ((A*B)_trans - B_trans * A_trans) = %e\n",MatNorm(D,n,n));
}

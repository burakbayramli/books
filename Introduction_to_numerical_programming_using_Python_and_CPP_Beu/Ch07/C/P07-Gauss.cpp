// Solves linear system by Gauss elimination
#include <stdio.h>
#include "memalloc.h"
#include "linsys.h"
#include "matutil.h"

int main()
{
   double **a, **b, det;
   int m, n;

   n = 4;                                                  // order of system
   m = 1;                                       // number of constant vectors
   a = Matrix(1,n,1,n);                                      // system matrix
   b = Matrix(1,n,1,m);                                     // constant terms

   a[1][1] = 1; a[1][2] = 2; a[1][3] = 3; a[1][4] = 4; b[1][1] = 30;
   a[2][1] = 2; a[2][2] = 1; a[2][3] = 2; a[2][4] = 3; b[2][1] = 22;
   a[3][1] = 3; a[3][2] = 2; a[3][3] = 1; a[3][4] = 2; b[3][1] = 18;
   a[4][1] = 4; a[4][2] = 3; a[4][3] = 2; a[4][4] = 1; b[4][1] = 20;
                                 // Solution: 1.0, 2.0, 3.0, 4.0; det = -20.0
   printf("A:\n");
   MatPrint(a,n,n);
   printf("B:\n");
   MatPrint(b,n,m);

   Gauss(a,b,n,m,det);                                        // solve system

   printf("det A = %6.3f\n",det);
   printf("Solution:\n");
   MatPrint(b,n,m);
}

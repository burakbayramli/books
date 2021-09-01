// Solves linear system by the Gauss-Seidel method
#include <stdio.h>
#include "memalloc.h"
#include "linsys.h"

int main()
{
   double **a, *b, *x, det, err;
   int i, n;

   n = 4;                                                  // order of system
   a = Matrix(1,n,1,n);                                      // system matrix
   b = Vector(1,n);                                         // constant terms
   x = Vector(1,n);                                               // solution

   a[1][1] = 1; a[1][2] = 2; a[1][3] = 3; a[1][4] = 4; b[1] = 30; x[1] = 0.9;
   a[2][1] = 2; a[2][2] = 1; a[2][3] = 2; a[2][4] = 3; b[2] = 22; x[2] = 1.5;
   a[3][1] = 3; a[3][2] = 2; a[3][3] = 1; a[3][4] = 2; b[3] = 18; x[3] = 2.9;
   a[4][1] = 4; a[4][2] = 3; a[4][3] = 2; a[4][4] = 1; b[4] = 20; x[4] = 4.1;
                                 // Solution: 1.0, 2.0, 3.0, 4.0; det = -20.0

   GaussSeidel(a,b,x,n,0,err);

   printf("Solution:\n");
   for (i=1; i<=n; i++) printf("%10.3f",x[i]);
   printf("\n");
   printf("Max. relative error = %6.1e\n",err);
}

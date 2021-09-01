// Eigenvalues and eigenvectors of symmetric matrices by the Jacobi method
#include <stdio.h>
#include "eigsys.h"

int main()
{
   double **a, **x, *d;
   double err, t;
   int i, j, k, n;

   n = 4;

   a = Matrix(1,n,1,n);
   x = Matrix(1,n,1,n);
   d = Vector(1,n);

   a[1][1] = 1;                                             // lower triangle
   a[2][1] = 2; a[2][2] = 1;
   a[3][1] = 3; a[3][2] = 2; a[3][3] = 1;
   a[4][1] = 4; a[4][2] = 3; a[4][3] = 2; a[4][4] = 1;
//	Eigenvalues: -3.414214, -1.099019, -0.585786, 9.099020

   for (i=2; i<=n; i++)                            // complete upper triangle
      for (j=1; j<=(i-1); j++) a[j][i] = a[i][j];  // - not altered by Jacobi

   Jacobi(a,x,d,n);
   EigSort(x,d,n,1);                     // sort eigenvalues and eigenvectors

   printf("Eigenvalues:\n");
   for (i=1; i<=n; i++) printf("%10.5f",d[i]);
   printf("\n");

   for (i=2; i<=n; i++)                            // restore original matrix
      for (j=1; j<=(i-1); j++) a[i][j] = a[j][i];  // from upper triangle

   err = 0e0;                                               // accuracy check
   for (i=1; i<=n; i++)
      for (j=1; j<=n; j++) {
         t = -x[i][j] * d[j];
         for (k=1; k<=n; k++) t += a[i][k] * x[k][j];
         if (err < fabs(t)) err = fabs(t);       // err = max|a x - lambda x|
      }
   printf("\nMaximum error = %9.2e\n",err);
}

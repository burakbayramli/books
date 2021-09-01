// Eigenvalues and eigenvectors of inverse of symmetric matrix
#include <stdio.h>
#include <time.h>
#include "linsys.h"
#include "eigsys.h"
#include "matutil.h"

int main()
{
   double **a, **a1, **x, **x1, *d, *d1;
   double det, norm;
   int i, j, n;

   n = 100;                                                // order of matrix
   a  = Matrix(1,n,1,n);                                // coefficient matrix
   a1 = Matrix(1,n,1,n);                                      // inverse of a
   x  = Matrix(1,n,1,n);                                 // eigenvectors of a
   x1 = Matrix(1,n,1,n);                                // eigenvectors of a1
   d = Vector(1,n); d1 = Vector(1,n);              // eigenvalues of a and a1

   srand(time(NULL));                   // initialize random number generator
   for (i=1; i<=n; i++)                 // generate random coefficient matrix
      for (j=1; j<=n; j++) {
         a[i][j] = a[j][i] = (double)rand()/RAND_MAX;
         a1[i][j] = a1[j][i] = a[i][j];                       // copy a in a1
      }

   Jacobi(a,x,d,n);                         // solve eigenvalue problem for a
   EigSort(x,d,n,1);                     // sort eigenvalues and eigenvectors

   MatInv(a1,n,det);                                           // a1 = a^(-1)
   Jacobi(a1,x1,d1,n);                  // solve eigenvalue problem of a^(-1)
   for (i=1; i<=n; i++) d1[i] = 1e0/d1[i]; // invert eigvalues before sorting
   EigSort(x1,d1,n,1);                   // sort eigenvalues and eigenvectors

   for (j=1; j<=n; j++)                             // loop over eigenvectors
      if (x[1][j] * x1[1][j] < 0)        // eigenvectors have different sign?
         for (i=1; i<=n; i++) x1[i][j] = -x1[i][j];             // match sign

   VecDiff(d,d1,d1,n);                     // difference of eigenvalues in d1
   norm = VecNorm(d1,n);
   printf("Norm of eigenvalue difference = %9.2e\n",norm);

   MatDiff(x,x1,x1,n,n);                  // difference of eigenvectors in x1
   norm = MatNorm(x1,n,n);
   printf("Norm of eigenvector difference = %9.2e\n",norm);
}

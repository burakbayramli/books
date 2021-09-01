// Hueckel method for benzene
#include <stdio.h>
#include "eigsys.h"
#include "matutil.h"

int main()
{
   double **H, **C, *E;
   double alpha, beta, err, t;
   int i, j, k, n;

   n = 6;

   H = Matrix(1,n,1,n);                                 // Hamiltonian matrix
   C = Matrix(1,n,1,n);                             // molecular coefficients
   E = Vector(1,n);                                          // eigenenergies

   alpha = -6.9;                // Coulomb & resonance integrals: F. Brogli &
   beta = -3.2;                 // E. Heilbronner, Theoret. Chim. Acta 1972

   for (i=1; i<=n; i++)
      for (j=1; j<=n; j++) H[i][j] = 0e0;
   for (i=1; i<=n; i++) H[i][i] = alpha;                 // diagonal elements
   for (i=1; i<=n-1; i++) {                          // off-diagonal elements
      H[i+1][i] = beta; H[i][i+1] = beta;
   }
   H[1][6] = beta; H[6][1] = beta;              // cyclic boundary conditions
                                                       // exact eigenenergies
   double X[] = { 0, alpha+2*beta, alpha+beta, alpha+beta,
                     alpha-beta, alpha-beta, alpha-2*beta };
   printf("Eigenenergies (exact):\n");
   VecPrint(X,n);

   Jacobi(H,C,E,n);
   EigSort(C,E,n,1);                     // sort eigenvalues and eigenvectors

   printf("Eigenenergies (eV):\n");
   VecPrint(E,n);

   printf("Eigenvextors:\n");
   MatPrint(C,n,n);

   for (i=2; i<=n; i++)                                // restore Hamiltonian
      for (j=1; j<=(i-1); j++) H[i][j] = H[j][i];      // from upper triangle

   err = 0e0;
   for (i=1; i<=n; i++)
      for (j=1; j<=n; j++) {
         t = - E[j] * C[i][j];
         for (k=1; k<=n; k++) t += H[i][k] * C[k][j];
         if (err < fabs(t)) err = fabs(t);       // err = max|a x - lambda x|
      }
   printf("\nMaximum error = %9.2e\n",err);
}

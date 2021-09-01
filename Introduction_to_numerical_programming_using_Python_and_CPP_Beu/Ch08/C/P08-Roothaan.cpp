// Solves the Roothaan equation F C = S C E for the H2O molecule
#include <stdio.h>
#include "eigsys.h"
#include "matutil.h"

int main()
{
   double **C, **F, **S, **S0;
   double *E, err, t;
   int i, j, k, n;
   FILE *f;

   n = 13;

   C = Matrix(1,n,1,n);                             // molecular coefficients
   F = Matrix(1,n,1,n);                                        // Fock matrix
   S = Matrix(1,n,1,n);                                     // overlap matrix
   S0 = Matrix(1,n,1,n);                     // backup copy of overlap matrix
   E = Vector(1,n);                                          // energy levels

   f = fopen("H2O.dat","r");                               // read in F and S
   if (f == NULL) { printf(" H2O.dat missing !\n"); exit(1); }
   while(fgetc(f) != '\n');                                    // skip header
   for (i=1; i<=n; i++)                        // read in lower triangle of F
      for (j=1; j<=i; j++) {                       // F not altered by EigSym
         fscanf(f,"%lf",&F[i][j]);
         F[j][i] = F[i][j];                                     // symmetrize
      }
   while(fgetc(f) != '\n');                               // finish last line

   while(fgetc(f) != '\n');                                    // skip header
   for (i=1; i<=n; i++)                        // read in lower triangle of S
      for (j=1; j<=i; j++) {                         // S destroyed by EigSym
         fscanf(f,"%lf",&S[i][j]);
         S[j][i] = S[i][j];                                     // symmetrize
         S0[i][j] = S0[j][i] = S[i][j];                           // backup S
      }
   fclose(f);

   printf("F: Fock matrix (sample)\n");
   MatPrint(F,5,5);
   printf("S: overlap matrix (sample)\n");
   MatPrint(S,5,5);

   EigSym(F,S,C,E,n);                              // solve Roothaan equation
   EigSort(C,E,n,1);                     // sort eigenvalues and eigenvectors

   printf("\nEigenenergies:\n");
   VecPrint(E,n);

   printf("\nHOMO-LUMO gap E[6]-E[5] = %4.3f Hartrees\n",E[6]-E[5]);

   err = 0e0;
   for (i=1; i<=n; i++)
      for (j=1; j<=n; j++) {
         t = 0e0;
         for (k=1; k<=n; k++) t += (F[i][k] - E[j]*S0[i][k]) * C[k][j];
         if (err < fabs(t)) err = fabs(t);       // err = max|a x - lambda x|
      }
   printf("\nMaximum error = %9.2e\n",err);
}

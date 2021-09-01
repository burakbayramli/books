//-------------------------------- linsys.h ---------------------------------
// Contains routines for solving systems of linear equations.
// Part of the numxlib numerics library. Author: Titus Beu, 2013
//---------------------------------------------------------------------------
#ifndef _LINSYS_
#define _LINSYS_

#include <stdio.h>
#include <math.h>
#include "memalloc.h"

//===========================================================================
void Gauss(double **a, double **b, int n, int m, double &det)
//---------------------------------------------------------------------------
// Solves matrix equation a x = b by Gaussian elimination with partial
// pivoting on columns.
// a   - coefficient matrix (n x n); destroyed on exit
// b   - matrix of constant terms (n x m); solution x on exit
// det - determinant of coefficient matrix (output).
//---------------------------------------------------------------------------
{
#define Swap(a,b) { t = a; a = b; b = t; }
   double amax, sum, t;
   int i, imax, j, k;

   det = 1e0;
   for (k=1; k<=n; k++) {                              // FORWARD ELIMINATION
      amax = 0e0;                                 // determine pivot row with
      for (i=k; i<=n; i++)                        // max. element on column k
         if (amax < fabs(a[i][k])) { amax = fabs(a[i][k]); imax = i; }
      if (amax == 0e0)
         { printf("Gauss: singular matrix !\n"); det = 0e0; return; }

      if (imax != k) {                         // interchange rows imax and k
         det = -det;                           // to put pivot on diagonal
         for (j=k; j<=n; j++) Swap(a[imax][j],a[k][j])
         for (j=1; j<=m; j++) Swap(b[imax][j],b[k][j])
      }

      det *= a[k][k];                      // multiply determinant with pivot

      t = 1e0/a[k][k];                           // divide pivot row by pivot
      for (j=k+1; j<=n; j++) a[k][j] *= t;
      for (j=  1; j<=m; j++) b[k][j] *= t;

      for (i=k+1; i<=n; i++) {                       // reduce non-pivot rows
         t = a[i][k];
         for (j=k+1; j<=n; j++) a[i][j] -= a[k][j]*t;
         for (j=  1; j<=m; j++) b[i][j] -= b[k][j]*t;
      }
   }

   for (k=n-1; k>=1; k--)                            // BACKWARD SUBSTITUTION
      for (j=1; j<=m; j++) {
         sum = b[k][j];
         for (i=k+1; i<=n; i++) sum -= a[k][i]*b[i][j];
         b[k][j] = sum;
      }
}

//===========================================================================
void GaussJordan0(double **a, double **b, int n, int m, double &det)
//---------------------------------------------------------------------------
// Solves matrix equation a x = b by Gauss-Jordan elimination with partial
// pivoting on columns.
// a   - coefficient matrix (n x n); destroyed on exit
// b   - matrix of constant terms (n x m); solution x on exit
// det - determinant of coefficient matrix (output).
//---------------------------------------------------------------------------
{
#define Swap(a,b) { t = a; a = b; b = t; }
   double amax, t;
   int i, imax, j, k;

   det = 1e0;
   for (k=1; k<=n; k++) {                              // FORWARD ELIMINATION
      amax = 0e0;                                 // determine pivot row with
      for (i=k; i<=n; i++)                        // max. element on column k
         if (amax < fabs(a[i][k])) { amax = fabs(a[i][k]); imax = i; }
      if (amax == 0e0)
         { printf("GaussJordan0: singular matrix !\n"); det = 0e0; return; }

      if (imax != k) {                         // interchange rows imax and k
         det = -det;                           // to put pivot on diagonal
         for (j=k; j<=n; j++) Swap(a[imax][j],a[k][j])
         for (j=1; j<=m; j++) Swap(b[imax][j],b[k][j])
      }

      det *= a[k][k];                      // multiply determinant with pivot

      t = 1e0/a[k][k];                           // divide pivot row by pivot
      for (j=k+1; j<=n; j++) a[k][j] *= t;
      for (j=  1; j<=m; j++) b[k][j] *= t;

      for (i=1; i<=n; i++)                           // reduce non-pivot rows
         if (i != k) {
            t = a[i][k];
            for (j=1; j<=n; j++) a[i][j] -= a[k][j]*t;
            for (j=1; j<=m; j++) b[i][j] -= b[k][j]*t;
         }
   }
}

//===========================================================================
void GaussJordan1(double **a, double **b, int n, int m, double &det)
//---------------------------------------------------------------------------
// Solves matrix equation a x = b by Gauss-Jordan elimination with partial
// pivoting on columns.
// a   - coefficient matrix (n x n); a^(-1) on exit
// b   - matrix of constant terms (n x m); solution x on exit
// det - determinant of coefficient matrix (output).
//---------------------------------------------------------------------------
{
#define Swap(a,b) { t = a; a = b; b = t; }
   double amax, t;
   int i, imax, j, k;
   int *ipivot;

   ipivot = IVector(1,n);                                // stores pivot rows

   det = 1e0;
   for (k=1; k<=n; k++) {                              // FORWARD ELIMINATION
      amax = 0e0;                                 // determine pivot row with
      for (i=k; i<=n; i++)                        // max. element on column k
         if (amax < fabs(a[i][k])) { amax = fabs(a[i][k]); imax = i; }
      if (amax == 0e0)
         { printf("GaussJordan1: singular matrix !\n"); det = 0e0; return; }
      ipivot[k] = imax;                              // store pivot row index

      if (imax != k) {                         // interchange rows imax and k
         det = -det;                           // to put pivot on diagonal
         for (j=1; j<=n; j++) Swap(a[imax][j],a[k][j])
         for (j=1; j<=m; j++) Swap(b[imax][j],b[k][j])
      }

      det *= a[k][k];                      // multiply determinant with pivot

      t = 1e0/a[k][k];                           // divide pivot row by pivot
      a[k][k] = 1e0;                       // diagonal element of unit matrix
      for (j=1; j<=n; j++) a[k][j] *= t;
      for (j=1; j<=m; j++) b[k][j] *= t;

      for (i=1; i<=n; i++)                           // reduce non-pivot rows
         if (i != k) {
            t = a[i][k];
            a[i][k] = 0e0;             // non-diagonal element of unit matrix
            for (j=1; j<=n; j++) a[i][j] -= a[k][j]*t;
            for (j=1; j<=m; j++) b[i][j] -= b[k][j]*t;
      }
   }

   for (k=n; k>=1; k--) {                     // rearrange columns of inverse
      imax = ipivot[k];
      if (imax != k)
         for (i=1; i<=n; i++) Swap(a[i][imax],a[i][k])
   }

   FreeIVector(ipivot,1);
}

//===========================================================================
void GaussJordan(double **a, double **b, int n, int m, double &det)
//---------------------------------------------------------------------------
// Solves matrix equation a x = b by Gauss-Jordan elimination with complete
// pivoting.
// a   - coefficient matrix (n x n); a^(-1) on exit
// b   - matrix of constant terms (n x m); solution x on exit
// det - determinant of coefficient matrix (output).
//---------------------------------------------------------------------------
{
#define Swap(a,b) { t = a; a = b; b = t; }
   double amax, t;
   int i, imax, j, jmax, k;
   int *ipivot, *jpivot, *npivot;

   ipivot = IVector(1,n); jpivot = IVector(1,n);    // pivot rows and columns
   npivot = IVector(1,n);                         // marks used pivot columns

   for (i=1; i<=n; i++) npivot[i] = 0;

   det = 1e0;
   for (k=1; k<=n; k++) {                              // FORWARD ELIMINATION
      amax = 0e0;                                          // determine pivot
      for (i=1; i<=n; i++) {                                // loop over rows
         if (npivot[i] != 1)
            for (j=1; j<=n; j++) {                       // loop over columns
               if (npivot[j] == 0)                  // pivoting not yet done?
                  if (amax < fabs(a[i][j]))
                     { amax = fabs(a[i][j]); imax = i; jmax = j; }
               if (npivot[j] > 1) {
                  printf("GaussJordan: singular matrix 1 !\n");
                  det = 0e0; return;
               }
            }
      }
      if (amax == 0e0)
         { printf("GaussJordan: singular matrix 2 !\n"); det = 0e0; return; }

      ipivot[k] = imax; jpivot[k] = jmax;       // store pivot row and column
      ++(npivot[jmax]);                             // mark used pivot column

      if (imax != jmax) {                   // interchange rows imax and jmax
         det = -det;                        // to put pivot on diagonal
         for (j=1; j<=n; j++) Swap(a[imax][j],a[jmax][j])
         for (j=1; j<=m; j++) Swap(b[imax][j],b[jmax][j])
      }
      det *= a[jmax][jmax];                // multiply determinant with pivot

      t = 1e0/a[jmax][jmax];                     // divide pivot row by pivot
      a[jmax][jmax] = 1e0;                 // diagonal element of unit matrix
      for (j=1; j<=n; j++) a[jmax][j] *= t;
      for (j=1; j<=m; j++) b[jmax][j] *= t;

      for (i=1; i<=n; i++)                           // reduce non-pivot rows
         if (i != jmax) {
            t = a[i][jmax];
            a[i][jmax] = 0e0;          // non-diagonal element of unit matrix
            for (j=1; j<=n; j++) a[i][j] -= a[jmax][j]*t;
            for (j=1; j<=m; j++) b[i][j] -= b[jmax][j]*t;
         }
   }

   for (k=n; k>=1; k--) {                     // rearrange columns of inverse
      imax = ipivot[k]; jmax = jpivot[k];
      if (imax != jmax)
         for (i=1; i<=n; i++) Swap(a[i][imax],a[i][jmax])
   }

   FreeIVector(ipivot,1); FreeIVector(jpivot,1);
   FreeIVector(npivot,1);
}

//===========================================================================
void LUFactor(double **a, int ipivot[], int n, double &det)
//---------------------------------------------------------------------------
// Performs LU factorization of (n x n) matrix a (diag(L) = 1). On exit,
// replaces upper triangle and diagonal with U, and lower triangle, with L.
// Uses partial pivoting on columns.
// a      - coefficient matrix (n x n); LU decomposition on exit
// ipivot - array of pivot row indexes (output)
// det    - determinant of coefficient matrix (output).
//---------------------------------------------------------------------------
{
   double amax, sum, t;
   int i, imax, j, k;

   det = 1e0;
   for (j=1; j<=n; j++) {                                // loop over columns
      for (i=1; i<=(j-1); i++) {                      // elements of matrix U
         sum = a[i][j];
         for (k=1; k<=(i-1); k++) sum -= a[i][k]*a[k][j];
         a[i][j] = sum;
      }
      amax = 0e0;
      for (i=j; i<=n; i++) {                          // elements of matrix L
         sum = a[i][j];                               // undivided by pivot
         for (k=1; k<=(j-1); k++) sum -= a[i][k]*a[k][j];
         a[i][j] = sum;
                                                           // determine pivot
         if (amax < fabs(a[i][j])) {amax = fabs(a[i][j]); imax = i;}
      }
      if (amax == 0e0)
         { printf("LUFactor: singular matrix !\n"); det = 0e0; return; }

      ipivot[j] = imax;                              // store pivot row index
                                               // interchange rows imax and j
      if (imax != j) {                         // to put pivot on diagonal
         det = -det;
         for (k=1; k<=n; k++)
            { t = a[imax][k]; a[imax][k] = a[j][k]; a[j][k] = t; }
      }

      det *= a[j][j];                      // multiply determinant with pivot

      t = 1e0/a[j][j];                       // divide elements of L by pivot
      for (i=j+1; i<=n; i++) a[i][j] *= t;
   }
}

//===========================================================================
void LUSystem(double **a, int ipivot[], double b[], int n)
//---------------------------------------------------------------------------
// Solves linear system a x = b of order n by LU factorization.
// a      - LU decomposition of coefficient matrix (returned by LUFactor)
// ipivot - array of pivot row indexes (input)
// b      - vector of constant terms (input); solution x (on exit)
//---------------------------------------------------------------------------
{
   double sum;
   int i, j;

   for (i=1; i<=n; i++) {                                    // solves Ly = b
      sum = b[ipivot[i]];
      b[ipivot[i]] = b[i];
      for (j=1; j<=(i-1); j++) sum -= a[i][j]*b[j];
      b[i] = sum;
   }

   for (i=n; i>=1; i--) {                                    // solves Ux = y
      sum = b[i];
      for (j=i+1; j<=n; j++) sum -= a[i][j]*b[j];
      b[i] = sum/a[i][i];
   }
}

//===========================================================================
void MatInv(double **a, int n, double &det)
//---------------------------------------------------------------------------
// Calculates inverse of real matrix by LU factorization.
// a   - (n x n) matrix (input); a^(-1) (output)
// det - determinant of coefficient matrix (output).
// Calls: LUFactor, LUSystem.
//---------------------------------------------------------------------------
{
   double **ainv, *b;
   int *ipivot, i, j;

   ainv = Matrix(1,n,1,n);            // temporary storage for inverse matrix
   b = Vector(1,n);
   ipivot = IVector(1,n);                                // stores pivot rows

   LUFactor(a,ipivot,n,det);                         // LU factorization of a
   if (det == 0e0)                                         // singular matrix
      { printf("MatInv: singular matrix !\n"); return; }

   for (j=1; j<=n; j++) {                 // loop over columns of unit matrix
      for (i=1; i<=n; i++) b[i] = 0e0;                            // column j
      b[j] = 1e0;                          
      LUSystem(a,ipivot,b,n);                                 // solve system
      for (i=1; i<=n; i++) ainv[i][j] = b[i];          // column j of inverse
   }

   for (j=1; j<=n; j++)                                  // copy inverse in a
      for (i=1; i<=n; i++) a[i][j] = ainv[i][j];

   FreeMatrix(ainv,1,1); FreeVector(b,1);
   FreeIVector(ipivot,1);
}

//===========================================================================
int MatTriInv(double **a, int n)
//---------------------------------------------------------------------------
// Calculates inverse of lower triangular matrix L of order n contained in
// (n x n) array a and replaces on exit lower triangle of a by L^(-1).
// Error flag = 0 - normal execution
//              1 - singular matrix.
//---------------------------------------------------------------------------
{
   double sum;
   int i, j, k;

   for (j=1; j<=n; j++) {                                // loop over columns
      if (a[j][j] <= 0e0) 
         { printf("MatTriInv: singular matrix !\n"); return 1; }
      a[j][j] = 1e0/a[j][j];                              // diagonal element

      for (i=j+1; i<=n; i++) {                       // non-diagonal elements
         sum = 0e0;
         for (k=j; k<=(i-1); k++) sum -= a[i][k]*a[k][j];
         a[i][j] = sum/a[i][i];
      }
   }
   return 0;
}

//===========================================================================
int Cholesky(double **a, int n, double &det)
//---------------------------------------------------------------------------
// Performs Cholesky factorization L * LT of real symmetric positive-definite
// (n x n) matrix a. On exit, replaces lower triangle of a with L.
// a   - coefficient matrix (n x n); L in lower triangle and diagonal on exit
// det - determinant of coefficient matrix (output).
// Error flag = 0 - normal execution
//              1 - a is not positive-definite.
//---------------------------------------------------------------------------
{
   double sum;
   int i, j, k;

   det = 1e0;
   for (j=1; j<=n; j++) {                                // loop over columns
      sum = a[j][j];                                      // diagonal element
      for (k=1; k<=(j-1); k++) sum -= a[j][k]*a[j][k];
      if (sum <= 0e0)
         { printf("Cholesky: matrix not positive-definite !\n"); return 1; }
      a[j][j] = sqrt(sum);

      for (i=j+1; i<=n; i++) {                       // non-diagonal elements
         sum = a[i][j];
         for (k=1; k<=(j-1); k++) sum -= a[i][k]*a[j][k];
         a[i][j] = sum / a[j][j];
      }

      det *= a[j][j]*a[j][j];
   }
   return 0;
}

//===========================================================================
void CholeskySys(double **a, double b[], int n)
//---------------------------------------------------------------------------
// Solves linear system a x = b with real symmetric positive-definite (n x n)
// matrix by Cholesky factorization L * LT.
// a - matrix L in lower triangle and on diagonal (returned by Cholesky)
// b - vector of constant terms (input); solution x (on exit)
//---------------------------------------------------------------------------
{
   double sum;
   int i, j;

   for (i=1; i<=n; i++) {                                    // solves Ly = b
      sum = b[i];
      for (j=1; j<=(i-1); j++) sum -= a[i][j]*b[j];
      b[i] = sum/a[i][i];
   }

   for (i=n; i>=1; i--) {                                  // solves LT x = y
      sum = b[i];
      for (j=i+1; j<=n; j++) sum -= a[j][i]*b[j];
      b[i] = sum/a[i][i];
   }
}

//===========================================================================
void MatSymInv(double **a, int n, double &det)
//---------------------------------------------------------------------------
// Calculates inverse of symmetric positive-definite (n x n) matrix by
// Cholesky factorization a = L * LT as a^(-1) = (L^(-1))T * L^(-1).
// L^(-1) is calculated using MatTriInv.
// a   - (n x n) matrix (input); a^(-1) (output)
// det - determinant of coefficient matrix (output).
// Calls: Cholesky, MatTriInv.
//---------------------------------------------------------------------------
{
   double sum;
   int ierr, i, j, k;

   ierr = Cholesky(a,n,det);                     // L * LT factorization of a
   if (ierr == 1)
      { printf("MatSymInv: matrix not positive-definite !\n"); return; }
   if (det == 0e0) { printf("MatSymInv: singular matrix !\n"); return; }

   MatTriInv(a,n);                                // L^(-1) in lower triangle

   for (i=1; i<=n; i++)                                 // (L^(-1))T * L^(-1)
      for (j=1; j<=i; j++) {
         sum = 0e0;
         for (k=i; k<=n; k++) sum += a[k][i] * a[k][j];
         a[j][i] = sum;                            // store in upper triangle
      }

   for (i=1; i<=n; i++)                            // complete lower triangle
      for (j=1; j<=i; j++) a[i][j] = a[j][i];
}

//===========================================================================
void TriDiagSys(double a[], double b[], double c[], double d[], int n)
//---------------------------------------------------------------------------
// Solves a system with tridiagonal matrix by LU factorization (diag(L) = 1).
// a - lower codiagonal (i=2,n)
// b - main diagonal (i=1,n)
// c - upper codiagonal (i=1,n-1)
// d - constant terms (i=1,n); solution on exit
// n - order of system.
//---------------------------------------------------------------------------
{
   int i;

   if (b[1] == 0e0) { printf("TriDiagSys: singular matrix !\n"); return; }
   for (i=2; i<=n; i++) {                                    // factorization
      a[i] /= b[i-1];
      b[i] -= a[i]*c[i-1];
      if (b[i] == 0e0) { printf("TriDiagSys: singular matrix !\n"); return; }
      d[i] -= a[i]*d[i-1];
   }

   d[n] /= b[n];                                     // backward substitution
   for (i=(n-1); i>=1; i--) d[i] = (d[i] - c[i]*d[i+1])/b[i];
}

//===========================================================================
void TriDiagSys(dcmplx a[], dcmplx b[], dcmplx c[], dcmplx d[], int n)
//---------------------------------------------------------------------------
// Solves a complex system with tridiagonal matrix by LU factorization.
// a - lower codiagonal (i=2,n)
// b - main diagonal (i=1,n)
// c - upper codiagonal (i=1,n-1)
// d - constant terms (i=1,n); solution on exit
// n - order of system.
//---------------------------------------------------------------------------
{
   int i;

   if (b[1] == 0e0) { printf("TriDiagSys: singular matrix !\n"); return; }
   for (i=2; i<=n; i++) {                                    // factorization
      a[i] /= b[i-1];
      b[i] -= a[i]*c[i-1];
      if (b[i] == 0e0) { printf("TriDiagSys: singular matrix !\n"); return; }
      d[i] -= a[i]*d[i-1];
   }

   d[n] /= b[n];                                     // backward substitution
   for (i=(n-1); i>=1; i--) d[i] = (d[i] - c[i]*d[i+1])/b[i];
}

//===========================================================================
void GaussSeidel(double **a, double b[], double x[], int n, int init,
                 double &err)
//---------------------------------------------------------------------------
// Solves linear system a x = b by the Gauss-Seidel method.
// To ensure convergence, the system is left-multiplied with a^T.
// a    - coefficient matrix (n x n)
// b    - vector of constant terms
// x    - initial approximation of solution (input); solution (output)
// n    - order of system
// err  - maximum relative error of the solution components
// init - initialization option: 0 - refines initial approximation 
//                               1 - initializes solution
//---------------------------------------------------------------------------
{
   const double eps = 1e-10;                  // relative precision criterion
   const int itmax = 1000;                           // max no. of iterations
   double del, f, **s, *t;
   int i, j, k;

   s = Matrix(1,n,1,n);                         // matrices of reduced system
   t = Vector(1,n);

   for (i=1; i<=n; i++) {                        // matrices of normal system
      for (j=1; j<=i; j++) {                     // by multiplication with aT
         s[i][j] = 0e0;                          // store result in s and t
         for (k=1; k<=n; k++) s[i][j] += a[k][i]*a[k][j];
         s[j][i] = s[i][j];
      }
      t[i] = 0e0;
      for (j=1; j<=n; j++) t[i] += a[j][i]*b[j];
   }

   for (i=1; i<=n; i++) {               // matrices s and t of reduced system
      f = -1e0/s[i][i]; t[i] /= s[i][i];
      for (j=1; j<=n; j++) s[i][j] *= f;
   }

   if (init) for (i=1; i<=n; i++) x[i] = t[i];         // initialize solution

   for (k=1; k<=itmax; k++) {                           // loop of iterations
      err = 0e0;
      for (i=1; i<=n; i++) {
         del = t[i];                                            // correction
         for (j=1; j<=n; j++) del += s[i][j]*x[j];
         x[i] += del;                        // new approximation to solution
         if (x[i]) del /= x[i];                             // relative error
         if (fabs(del) > err) err = fabs(del);               // maximum error
      }
      if (err <= eps) break;                             // check convergence
   }
   if (k > itmax) printf("GaussSeidel: max. no. of iterations exceeded !\n");

   FreeMatrix(s,1,1); FreeVector(t,1);
}

#endif

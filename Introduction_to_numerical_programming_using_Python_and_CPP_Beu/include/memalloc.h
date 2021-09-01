//------------------------------- memalloc.h --------------------------------
// Functions for dynamic memory allocation of vectors and matrices.
// Part of the numxlib numerics library. Author: Titus Beu, 2013
//---------------------------------------------------------------------------
#ifndef _MEMALLOC_
#define _MEMALLOC_

#include <stdlib.h>
#include <stdio.h>
#include <complex>

using namespace std;
typedef complex<double> dcmplx;

//===========================================================================
double *Vector(int imin, int imax)
//---------------------------------------------------------------------------
// Allocates a double vector with indices in the range [imin,imax]
//---------------------------------------------------------------------------
{
   double *p;
                                       // assign block start to array pointer
   p = (double*) malloc((size_t) ((imax-imin+1)*sizeof(double)));
   if (!p) {
      printf("Vector: allocation error !\n");
      exit(1);
   }
   return p - imin;                                      // adjust for offset
}

//===========================================================================
void FreeVector(double *p, int imin)
//---------------------------------------------------------------------------
// Deallocates a double vector allocated with Vector, with offset imin
//---------------------------------------------------------------------------
{
   free((void*) (p+imin));                           // compensate for offset
}

//===========================================================================
double **Matrix(int imin, int imax, int jmin, int jmax)
//---------------------------------------------------------------------------
// Allocates a double matrix, with row and column indices in the range
// [imin,imax] x [jmin,jmax]
//---------------------------------------------------------------------------
{
   int i, ni = imax-imin+1, nj = jmax-jmin+1;  // numbers of rows and columns
   double **p;
                                            // allocate array of row pointers
   p = (double**) malloc((size_t)(ni*sizeof(double*)));
   if (!p) {
      printf("Matrix: level 1 allocation error !\n");
      exit(1);
   }
   p -= imin;                                        // adjust for row offset
                                     // assign block start to 1st row pointer
   p[imin] = (double*) malloc((size_t)(ni*nj*sizeof(double)));
   if (!p[imin]) {
      printf("Matrix: level 2 allocation error !\n");
      exit(2);
   }
   p[imin] -= jmin;               // adjust 1st row pointer for column offset
                                  // define row pointers spaced by row length
   for (i=imin+1; i<=imax; i++) p[i] = p[i-1] + nj;

   return p;
}

//===========================================================================
void FreeMatrix(double **p, int imin, int jmin)
//---------------------------------------------------------------------------
// Deallocates a double matrix allocated with Matrix, with row and column
// offsets imin and jmin
//---------------------------------------------------------------------------
{
   free((void*) (p[imin]+jmin));                          // deallocate block
   free((void*) (p+imin));                // deallocate array of row pointers
}

//===========================================================================
int *IVector(int imin, int imax)
//---------------------------------------------------------------------------
// Allocates an int vector with indices in the range [imin,imax]
//---------------------------------------------------------------------------
{
   int *p;
                                       // assign block start to array pointer
   p = (int*) malloc((size_t) ((imax-imin+1)*sizeof(int)));
   if (!p) {
      printf("IVector: allocation error !\n");
      exit(1);
   }
   return p - imin;                                      // adjust for offset
}

//===========================================================================
void FreeIVector(int *p, int imin)
//---------------------------------------------------------------------------
// Deallocates an int vector allocated with IVector, with offset imin
//---------------------------------------------------------------------------
{
   free((void*) (p+imin));                           // compensate for offset
}

//===========================================================================
int **IMatrix(int imin, int imax, int jmin, int jmax)
//---------------------------------------------------------------------------
// Allocates an int matrix, with row and column indices in the range
// [imin,imax] x [jmin,jmax]
//---------------------------------------------------------------------------
{
   int i, ni = imax-imin+1, nj = jmax-jmin+1;  // numbers of rows and columns
   int **p;
                                            // allocate array of row pointers
   p = (int**) malloc((size_t)(ni*sizeof(int*)));
   if (!p) {
      printf("Matrix: level 1 allocation error !\n");
      exit(1);
   }
   p -= imin;                                        // adjust for row offset
                                     // assign block start to 1st row pointer
   p[imin] = (int*) malloc((size_t)(ni*nj*sizeof(int)));
   if (!p[imin]) {
      printf("Matrix: level 2 allocation error !\n");
      exit(2);
   }
   p[imin] -= jmin;               // adjust 1st row pointer for column offset
                                  // define row pointers spaced by row length
   for (i=imin+1; i<=imax; i++) p[i] = p[i-1] + nj;

   return p;
}

//===========================================================================
void FreeIMatrix(int **p, int imin, int jmin)
//---------------------------------------------------------------------------
// Deallocates an int matrix allocated with IMatrix, with row and column
// offsets imin and jmin
//---------------------------------------------------------------------------
{
   free((void*) (p[imin]+jmin));                          // deallocate block
   free((void*) (p+imin));                // deallocate array of row pointers
}

//===========================================================================
dcmplx *CVector(int imin, int imax)
//---------------------------------------------------------------------------
// Allocates a complex vector with indices in the range [imin,imax]
//---------------------------------------------------------------------------
{
   dcmplx *p;
                                       // assign block start to array pointer
   p = (dcmplx*) malloc((size_t) ((imax-imin+1)*2*sizeof(double)));
   if (!p) {
      printf("CVector: allocation error !\n");
      exit(1);
   }
   return p - imin;                                      // adjust for offset
}

//===========================================================================
void FreeCVector(dcmplx *p, int imin)
//---------------------------------------------------------------------------
// Deallocates a complex vector allocated with CVector, with offset imin
//---------------------------------------------------------------------------
{
   free((void*) (p+imin));                           // compensate for offset
}

//===========================================================================
dcmplx **CMatrix(int imin, int imax, int jmin, int jmax)
//---------------------------------------------------------------------------
// Allocates a complex matrix, with row and column indices in the range
// [imin,imax] x [jmin,jmax]
//---------------------------------------------------------------------------
{
   int i, ni = imax-imin+1, nj = jmax-jmin+1;  // numbers of rows and columns
   dcmplx **p;
                                            // allocate array of row pointers
   p = (dcmplx**) malloc((size_t)(ni*2*sizeof(double*)));
   if (!p) {
      printf("CMatrix: level 1 allocation error !\n");
      exit(1);
   }
   p -= imin;                                        // adjust for row offset
                                     // assign block start to 1st row pointer
   p[imin] = (dcmplx*) malloc((size_t)(ni*nj*2*sizeof(double)));
   if (!p[imin]) {
      printf("CMatrix: level 2 allocation error !\n");
      exit(2);
   }
   p[imin] -= jmin;               // adjust 1st row pointer for column offset
                                  // define row pointers spaced by row length
   for (i = imin+1; i <= imax; i++) p[i] = p[i-1] + nj;

   return p;
}

//===========================================================================
void FreeCMatrix(dcmplx **p, int imin, int jmin)
//---------------------------------------------------------------------------
// Deallocates a complex matrix allocated with CMatrix, with row and column
// offsets imin and jmin
//---------------------------------------------------------------------------
{
   free((void*) (p[imin]+jmin));                          // deallocate block
   free((void*) (p+imin));                // deallocate array of row pointers
}

#endif

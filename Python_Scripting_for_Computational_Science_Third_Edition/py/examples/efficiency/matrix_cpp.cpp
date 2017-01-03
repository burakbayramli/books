#include <matrix_cpp.h>
#include <stdlib.h>
#include <math.h>

Matrix::Matrix(int n_)
{
  n = n_;
  A = (double**) calloc(n, sizeof(double*));
  /* then allocate the storage of all matrix entries: */
  A[0] = (double*) calloc(n*n, sizeof(double));
  /* let the other row pointers point to the correct row: */
  for (int i=1; i<n; i++) {
    A[i] = A[0] + n*i;
  }
}

Matrix::~Matrix()
{
  free(A[0]);  /* free chunk of matrix entries*/
  free(A);     /* free array of pointers to rows */
}

void Matrix::fill1()
{
  int i,j;
  for (i=0; i<n; i++)
    for (j=0; j<n; j++)
      A[i][j] = i*j-2;
}

void Matrix::fill2()
{
  int i,j;
  double x,y;
  for (i=0; i<n; i++) {
    x = 0.1*i;
    for (j=0; j<n; j++) {
      y = 0.1*j;
      A[i][j] = sin(x)*sin(y)*exp(-x*y);
    }
  }
}


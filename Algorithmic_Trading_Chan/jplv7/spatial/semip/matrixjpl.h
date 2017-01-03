/*===================================================================
 matrixjpl.h
===================================================================*/


#ifdef NUMERICS_DLL
	#define NUMERICS_EXPORT __declspec (dllexport)
#else
	#define NUMERICS_EXPORT
#endif


#define NUMERICS_PI        3.14159265358979323846
#define NUMERICS_E         2.71828182845904523536
#define NUMERICS_EULER     0.5772156649
#define NUMERICS_ITMAX     100
#define NUMERICS_MAX_ERROR 5.0e-9
#define NUMERICS_FLOAT_MIN 1.0E-37
#define NUMERICS_FLOAT_MAX 1.0E+38

#ifndef BOOL
#define BOOL  int
#define FALSE 0
#define TRUE  1
#endif


NUMERICS_EXPORT void NUMERICS_ERROR(const char *msg);


NUMERICS_EXPORT BOOL choldcmp(double **a, int n, double *p);
/*-------------------------------------------------------------------
 Given a positive-definit symmetric matrix a[0..n-1][0..n-1], this
 routine constructs its Colesky decomposition, A = LL'.  On input,
 only the upper triangle of a need be given; it is not modified.
 the Cholesky factor L is returned in the lower traingle of a,
 except for its diagonal elements which are returned in p[0..n-1].
 The routine returns true if the inversion was successful, otherwise
 it returns FALSE.
-------------------------------------------------------------------*/

NUMERICS_EXPORT void cholsl(double **a, int n, double *p, double *b, double *x);
/*-------------------------------------------------------------------
 Solves the set of n linear equations Ax = b, where a is a positive-
 definite symmetric matrix.  a[0..n-1][0..n-1] and p[0..n-1] are
 input as the output of the routine choldcmp.  Only the lower
 triangle of a is accessed.  b[0..n-1] is input as the right-hand
 side vector.  The solution vector is returned in x[0..n-1].  a, n,
 and p are not modified and can be left in place for successive
 calls with different right-hand sides b.  b is not modified unless
 you identify b as x in the calling sequence, which is allowed.
-------------------------------------------------------------------*/

NUMERICS_EXPORT double determinant(double **a, int n);
/*-------------------------------------------------------------------
 Returns the determinant of the matrix a[0..n-1][0..n-1].  a is
 destroyed by this routine.
-------------------------------------------------------------------*/

NUMERICS_EXPORT BOOL inverse(double **a, int n);
/*-------------------------------------------------------------------
 Replaces the matrix a[0..n-1][0..n-1] with its inverse. The
 routine returns TRUE if the inversion was successful, otherwise it
 returns FALSE.
-------------------------------------------------------------------*/

enum {LINSOLVE_LU = 0x1};

NUMERICS_EXPORT BOOL linsolve(double **m, double *b, int n, int method);
/*-------------------------------------------------------------------
 Given a matrix mat[0..n-1][0..n-1] and vector b[0..n-1], the
 solution vector x is found for the linear system m.x = b.  The
 solution vector is returned in b.  The routine returns TRUE if the
 solution vector is successfully found, otherwise it returns FALSE.
 Both m and b are destroyed by this routine.
-------------------------------------------------------------------*/

NUMERICS_EXPORT void lubksb(double **mat, int n, int *indx, double *b);
/*-------------------------------------------------------------------
 Given a matrix mat[0..n-1][0..n-1] and permutation vector
 indx[0..n-1] returned from ludcmp, this routines solves the set of
 n linear equations mat.x = b.  b[0..n-1] is input as the
 right-hand side vector and returns the solution vector x.  a, n,
 and indx are not modified by this routine and can be left in place
 for successive colls with different right-hand sides b.
-------------------------------------------------------------------*/

NUMERICS_EXPORT BOOL ludcmp(double **mat, int n, int *indx, double *d);
/*-------------------------------------------------------------------
 Given a matrix a[0..n-1][0..n-1], this routines replaces it by the
 LU decomposition of a rowwise permutation of itself.  a and n are
 input.  a is output, with the diagonal elements of the lower
 triangular matrix are equal to 1.  indx[0..n-1] is an output
 vector that records the row permutation effected by the partial
 pivoting.  d is output as 1 or -1 depending on whether the number
 of row interchanges was even or odd, respectively.  This routine
 is used in combination with lubksb to solve linear equations or
 invert a matrix.  The routine returns TRUE if the decomposition
 was successful, otherwise it returns FALSE.
-------------------------------------------------------------------*/

NUMERICS_EXPORT void matmat(double **a, int nra, int nca, double **b, int ncb, double **prod);
/*-------------------------------------------------------------------
 Postmultiplies the matrix a[0..nra-1][0..nca-1] by the matrix
 b[0..nca-1][0..ncb-1] and returns the product in the matrix
 prod[0..nra-1][0..ncb-1].
-------------------------------------------------------------------*/

NUMERICS_EXPORT void matvec(double **a, int nra, int nca, double *x, double *b);
/*-------------------------------------------------------------------
 Postmultiplies the matrix a[0..nra-1][0..nca-1] by the vector
 x[0..nca-1] and returns the product in the vector b[0..nra-1].
-------------------------------------------------------------------*/

NUMERICS_EXPORT void transpose(double **a, int nr, int nc, double **at);
/*-------------------------------------------------------------------
 Returns the transpose of a[0..nr-1][0..nc-1] as
 at[0..nc-1][0..nr-1].
-------------------------------------------------------------------*/

NUMERICS_EXPORT void vecmat(double *x, double **a, int nra, int nca, double *b);
/*-------------------------------------------------------------------
 Premultiplies the matrix a[0..nra-1][0..nca-1] by the vector
 x[0..nra-1] and returns the product in the vector b[0..nca-1].
-------------------------------------------------------------------*/

NUMERICS_EXPORT double vecvec(double *first1, double* last1, double* first2);
/*-------------------------------------------------------------------
 Returns the inner product between the vectors u[0..n-1] and
 v[0..n-1].
-------------------------------------------------------------------*/

NUMERICS_EXPORT double **dmatrix(int nrl, int nrh, int ncl, int nch);
/*-------------------------------------------------------------------
 Allocate an double matrix with subscript range
 m[nrl..nrh][ncl..nch].
-------------------------------------------------------------------*/

NUMERICS_EXPORT void free_dmatrix(double **m, int nrl, int nrh, int ncl);
/*-------------------------------------------------------------------
 Deallocate an double matrix associated with dmatrix().
-------------------------------------------------------------------*/

NUMERICS_EXPORT int **imatrix(int nrl, int nrh, int ncl, int nch);
/*-------------------------------------------------------------------
 Allocate an int matrix with subscript range m[nrl..nrh][ncl..nch].
-------------------------------------------------------------------*/

NUMERICS_EXPORT void free_imatrix(int **m, int nrl, int nrh, int ncl);
/*-------------------------------------------------------------------
 Deallocate an int matrix associated with imatrix().
-------------------------------------------------------------------*/

NUMERICS_EXPORT double *dvector(int nl, int nh);
/*-------------------------------------------------------------------
 Allocate an double vector with subscript range v[nl..nh].
-------------------------------------------------------------------*/

NUMERICS_EXPORT void free_dvector(double *v, int nl);
/*-------------------------------------------------------------------
 Deallocate an double vector associated with dvector().
-------------------------------------------------------------------*/

NUMERICS_EXPORT int *ivector(int nl, int nh);
/*-------------------------------------------------------------------
 Allocate an int vector with subscript range v[nl..nh].
-------------------------------------------------------------------*/

NUMERICS_EXPORT void free_ivector(int *v, int nl);
/*-------------------------------------------------------------------
 Deallocate an int vector associated with ivector().
-------------------------------------------------------------------*/

extern double dabs(double in);
extern void meanvar(double x[], int n, double *mean, double *variance);
extern void matmulc(double z[], double x[], double y[], int size, int m1, int m2, int n1, int n2);
extern void matdivc(double z[], double x[], double y[], int size, int m1, int m2, int n1, int n2);
extern void mataddc(double z[], double x[], double y[], int size, int m1, int m2, int n1, int n2);
extern void matsubc(double z[], double x[], double y[], int size, int m1, int m2, int n1, int n2);

extern void permute(int n, int *p, double *data);
extern void permute_inverse(int n, int *p, double *data);

extern void vprint(int n, double *vec);
extern void ivprint(int n, int *vec);
extern void mprint(int n, int m, double **mat);
extern void imprint(int n, int m, int **mat);

extern void vextract(int n, int col, double **mat, double *vec);
extern void vinsert(int n, int col, double *vec, double **mat);
extern void ivextract(int n, int col, int **mat, int *vec);
extern void vcopy(int n, double *vec1, double *vec2);




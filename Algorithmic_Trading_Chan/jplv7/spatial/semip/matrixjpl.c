/*===================================================================
 matrixjpl.c

 carried out linear algebra routines

===================================================================*/

#include <math.h>
#include <malloc.h>
#include <stddef.h>
#include <stdio.h>
#include "matrixjpl.h"

double dabs(double in)
{
	double out;

	if (in > 0.0)
		out = in;
    if (in < 0.0)
		 out = -in;
    if (in == 0.0)
        out = 0.0;
  
return out;
}


void vextract(int n, int col, double **mat, double *vec)
{ // extracts column 'col' from a double matrix
	int i;
	// N.B. col is 0-indexed as in C

	for(i=0; i<n; i++)
		vec[i] = mat[i][col];
}

void vinsert(int n, int col, double *vec, double **mat)
{ // inserts vec into 'col' of a double matrix
	int i;
	// N.B. col is 0-indexed as in C

	for(i=0; i<n; i++)
		mat[i][col] = vec[i];
}

void ivextract(int n, int col, int **mat, int *vec)
{ // extracts column 'col' from an integer matrix
	int i;
	// N.B. col is 0-indexed as in C

	for(i=0; i<n; i++)
		vec[i] = mat[i][col];
}


void vcopy(int n, double *vec1, double *vec2)
{ // copies vec1 to vec2
	int i;

	for(i=0; i<n; i++)
		vec2[i] = vec1[i];
}




// vprint prints double vectors
void vprint(int n, double *vec)
{
int i;

for(i=0; i<n; i++)
printf(" %8.3lf \n",vec[i]);

printf("\n");

}

// ivprint prints integer vectors
void ivprint(int n, int *vec)
{
int i;

for(i=0; i<n; i++)
printf(" %8d \n",vec[i]);

printf("\n");

}


// mprint prints double matrices
void mprint(int n, int m, double **vec)
{
int i, j;

for(i=0; i<n; i++){
	for(j=0; j<m; j++)
	printf(" %8.3lf ",vec[i][j]);
	printf(" \n");
}

printf("\n");


}

// imprint prints integer matrices
void imprint(int n, int m, int **vec)
{
int i, j;

for(i=0; i<n; i++){
	for(j=0; j<m; j++)
	printf(" %8d ",vec[i][j]);
	printf(" \n");
}

printf("\n");

}

// permute() and permute_inverse() perform
/* In-place Permutations 

   permute:    OUT[i]       = IN[perm[i]]     i = 0 .. N-1
   invpermute: OUT[perm[i]] = IN[i]           i = 0 .. N-1

   PERM is an index map, i.e. a vector which contains a permutation of
   the integers 0 .. N-1.

   From Knuth "Sorting and Searching", Volume 3 (3rd ed), Section 5.2
   Exercise 10 (answers), p 617
*/

void permute(int n, int *p, double *data)
{
  int i, k, pk;
  double r1, t;
  
  for (i = 0; i < n; i++)
    {
      k = p[i];
      
      while (k > i) 
        k = p[k];
      
      if (k < i)
        continue ;
      
      /* Now have k == i, i.e the least in its cycle */
      
      pk = p[k];
      
      if (pk == i)
        continue ;
      
      /* shuffle the elements of the cycle */
      
      t = data[i];
      
      while (pk != i)
        {
          r1 = data[pk];
          data[k] = r1;
          k = pk;
          pk = p[k];
        };
      
      data[k] = t;
    }
}

void permute_inverse (int n, int *p, double *data)
{
  int i, k, pk;

  double r1, t;

  for (i = 0; i < n; i++)
    {
      k = p[i];
          
      while (k > i) 
        k = p[k];

      if (k < i)
        continue ;
      
      /* Now have k == i, i.e the least in its cycle */

      pk = p[k];

      if (pk == i)
        continue ;
      
      /* shuffle the elements of the cycle in the inverse direction */
      
      t = data[k];

      while (pk != i)
        {
          r1 = data[pk];
          data[pk] = t;
          k = pk;
          pk = p[k];
          t = r1;
        };
      
      data[pk] = t;
    }

}




void matmulc(double z[], double x[], double y[], int size,
             int m1, int m2, int n1, int n2)
{// mutliplies non-conformable matrices so long as they
 // are conformable in a least one dimension
  int i, j;
  int flag = 0;
  
  /* case of two equal matrices */
  if (m1 == m2  &&  n1 == n2){
  for (i=0; i <= size-1; i++){
  z[i] = x[i]*y[i];
  }
    flag = 1;
  }
  if (m1 == m2 && n2 == 1) {
  size = 0;
  for (j=0; j<= n1-1; j++){
  for (i=0; i<= m1-1; i++){
  z[size] = x[size] * y[i];
  size = size + 1;
  }
  }
      flag = 1;
  }
  if (m1 == m2 && n1 == 1) {
  size = 0;
  for (j=0; j<= n2-1; j++){
  for (i=0; i<= m1-1; i++){
  z[size] = y[size] * x[i];
  size = size + 1;
  }
  }
      flag = 1;
  }
  if (n1 == n2 && m2 == 1) {
  size = 0;
  for (i=0; i<= n1-1; i++){
  for (j=0; j<= m1-1; j++){
  z[size] = x[size] * y[i];
  size = size + 1;
  }
  }
      flag = 1;
  }
  
  if (n1 == n2 && m1 == 1) {
  size = 0;
  for (i=0; i<= n1-1; i++){
  for (j=0; j<= m2-1; j++){
  z[size] = y[size] * x[i];
  size = size + 1;
  }
  }
      flag = 1;
  }
  
   if (flag == 0){
      printf("matmulc: nonconformability of matrices \n");
   }

}

void matdivc(double z[], double x[], double y[], int size,
             int m1, int m2, int n1, int n2)
{// divides non-conformable matrices so long as they
 // are conformable in a least one dimension
  int i, j;
  int flag = 0;
  
  /* case of two equal matrices */
  if (m1 == m2  &&  n1 == n2){
  for (i=0; i <= size-1; i++){
  z[i] = x[i]/y[i];
  }
    flag = 1;
  }
  if (m1 == m2 && n2 == 1) {
  size = 0;
  for (j=0; j<= n1-1; j++){
  for (i=0; i<= m1-1; i++){
  z[size] = x[size] / y[i];
  size = size + 1;
  }
  }
      flag = 1;
  }
  if (m1 == m2 && n1 == 1) {
  size = 0;
  for (j=0; j<= n2-1; j++){
  for (i=0; i<= m1-1; i++){
  z[size] = y[size] / x[i];
  size = size + 1;
  }
  }
      flag = 1;
  }
  if (n1 == n2 && m2 == 1) {
  size = 0;
  for (i=0; i<= n1-1; i++){
  for (j=0; j<= m1-1; j++){
  z[size] = x[size] / y[i];
  size = size + 1;
  }
  }
      flag = 1;
  }
  
  if (n1 == n2 && m1 == 1) {
  size = 0;
  for (i=0; i<= n1-1; i++){
  for (j=0; j<= m2-1; j++){
  z[size] = y[size] / x[i];
  size = size + 1;
  }
  }
      flag = 1;
  }
  
   if (flag == 0){
      printf("matdivc: nonconformability of matrices \n");
   }

}

void mataddc(double z[], double x[], double y[], int size,
             int m1, int m2, int n1, int n2)
{// adds non-conformable matrices so long as they
 // are conformable in a least one dimension
  int i, j;
  int flag = 0;
  
  /* case of two equal matrices */
  if (m1 == m2  &&  n1 == n2){
  for (i=0; i <= size-1; i++){
  z[i] = x[i]+y[i];
  }
    flag = 1;
  }
  if (m1 == m2 && n2 == 1) {
  size = 0;
  for (j=0; j<= n1-1; j++){
  for (i=0; i<= m1-1; i++){
  z[size] = x[size] + y[i];
  size = size + 1;
  }
  }
      flag = 1;
  }
  if (m1 == m2 && n1 == 1) {
  size = 0;
  for (j=0; j<= n2-1; j++){
  for (i=0; i<= m1-1; i++){
  z[size] = y[size] + x[i];
  size = size + 1;
  }
  }
      flag = 1;
  }
  if (n1 == n2 && m2 == 1) {
  size = 0;
  for (i=0; i<= n1-1; i++){
  for (j=0; j<= m1-1; j++){
  z[size] = x[size] + y[i];
  size = size + 1;
  }
  }
      flag = 1;
  }
  
  if (n1 == n2 && m1 == 1) {
  size = 0;
  for (i=0; i<= n1-1; i++){
  for (j=0; j<= m2-1; j++){
  z[size] = y[size] + x[i];
  size = size + 1;
  }
  }
      flag = 1;
  }
  
   if (flag == 0){
      printf("mataddc: nonconformability of matrices \n");
   }

}

void matsubc(double z[], double x[], double y[], int size,
             int m1, int m2, int n1, int n2)
{// subtracts non-conformable matrices so long as they
 // are conformable in a least one dimension
  int i, j;
  int flag = 0;
  
  /* case of two equal matrices */
  if (m1 == m2  &&  n1 == n2){
  for (i=0; i <= size-1; i++){
  z[i] = x[i] - y[i];
  }
    flag = 1;
  }
  if (m1 == m2 && n2 == 1) {
  size = 0;
  for (j=0; j<= n1-1; j++){
  for (i=0; i<= m1-1; i++){
  z[size] = x[size] - y[i];
  size = size + 1;
  }
  }
      flag = 1;
  }
  if (m1 == m2 && n1 == 1) {
  size = 0;
  for (j=0; j<= n2-1; j++){
  for (i=0; i<= m1-1; i++){
  z[size] = y[size] - x[i];
  size = size + 1;
  }
  }
      flag = 1;
  }
  if (n1 == n2 && m2 == 1) {
  size = 0;
  for (i=0; i<= n1-1; i++){
  for (j=0; j<= m1-1; j++){
  z[size] = x[size] - y[i];
  size = size + 1;
  }
  }
      flag = 1;
  }
  
  if (n1 == n2 && m1 == 1) {
  size = 0;
  for (i=0; i<= n1-1; i++){
  for (j=0; j<= m2-1; j++){
  z[size] = y[size] - x[i];
  size = size + 1;
  }
  }
      flag = 1;
  }
  
   if (flag == 0){
      printf("matsubc: nonconformability of matrices \n");
   }

}

void meanvar(double data[], int n, double *ave, double *svar)
{ // computes mean and variance of a vector
int j;
double s;

*ave = (*svar) = 0.0;
for(j=0; j<n; j++) *ave += data[j];
*ave/= n;
for(j=0; j<n; j++){
   s = data[j]-(*ave);
   *svar += s*s;
   }
   *svar /= (n-1);
}

NUMERICS_EXPORT BOOL choldcmp(double **a, int n, double *p)
{
    int i, j, k;
    double sum;
    
    *p = sqrt(a[0][0]);
    for(i = 1; i < n; i++){
        a[i][0] /= *p;
    }
    for(i = 1; i < n; i++){
        sum = a[i][i];
        for(k = 0; k < i; ++k){
            sum -= a[i][k] * a[i][k];
        }
        if(sum <= 0.0){
            return FALSE;
        }
        *(p + i) = sqrt(sum);
        for(j = n - 1; j > i; --j){
            sum = a[j][i];
            for(k = 0; k < i; ++k){
                sum -= a[j][k] * a[i][k];
            }
            a[j][i] = sum / *(p + i);
        }
    }
    
    return TRUE;
}

NUMERICS_EXPORT void cholsl(double **a, int n, double *p, double *b, double *x)
{
    int i, k;
    double sum;
    
    for(i = 0; i < n; i++){
        for(sum = *(b + i), k = i - 1; k >= 0; --k){
            sum -= a[i][k] * *(x + k);
        }
        *(x + i) = sum / *(p + i);
    }
    for(i = n - 1; i >= 0; --i){
        for(sum = *(x + i), k = i + 1; k < n; ++k){
            sum -= a[k][i] * *(x + k);
        }
        *(x + i) = sum / *(p + i);
    }
}

NUMERICS_EXPORT double determinant(double **a, int n)
{
    double ret;
    int i;
    int *indx = ivector(0, n - 1);
    
    if(ludcmp(a, n, indx, &ret)){
        for(i = 0; i < n; i++){
            ret *= a[i][i];
        }
    } else {
        ret = 0.0;
    }
    
    free_ivector(indx, 0);
    
    return ret;
}

NUMERICS_EXPORT BOOL inverse(double **a, int n)
{
    double d;
    int i, j;
    BOOL ret = FALSE;
    double** ai = dmatrix(0, n - 1, 0, n - 1);
    double* col = dvector(0, n - 1);
    int* indx = ivector(0, n - 1);
    
    if(ludcmp(a, n, indx, &d)){
        for(j = 0; j < n; j++){
            for(i = 0; i < n; i++) col[i] = 0.0;
            col[j] = 1.0;
            lubksb(a, n, indx, col);
            for(i = 0; i < n; i++) ai[i][j] = col[i];
        }
        for(i = 0; i < n; i++){
            for(j = 0; j < n; j++){
                a[i][j] = ai[i][j];
            }
        }
        ret = TRUE;
    }
    
    free_dmatrix(ai, 0, n - 1, 0);
    free_dvector(col, 0);
    free_ivector(indx, 0);
    
    return ret;
}

NUMERICS_EXPORT BOOL linsolve(double **m, double *b, int n, int method)
{
    int* indx;
    double d;
    BOOL ret = FALSE;
    
    if(method | LINSOLVE_LU){
        indx = ivector(0, n - 1);
        ret = ludcmp(m, n, indx, &d);
        if(ret){
            lubksb(m, n, indx, b);
        }
        free_ivector(indx, 0);
    }
    
    return ret;
}

NUMERICS_EXPORT void lubksb(double **m, int n, int *indx, double *b)
{
    int i, ii = -1, ip, j;
    double sum;
    
    for(i = 0; i < n; i++){
        ip = *(indx + i);
        sum = *(b + ip);
        *(b + ip) = *(b + i);
        if(ii > -1){
            for(j = ii; j <= i - 1; j++){
                sum -= m[i][j] * *(b + j);
            }
        } else if(sum){
            ii = i;
        }
        *(b + i) = sum;
    }
    for(i = n - 1; i >= 0; i--){
        sum = *(b + i);
        for(j = i + 1; j < n; j++){
            sum -= m[i][j] * *(b + j);
        }
        *(b + i) = sum / m[i][i];
    }
}

NUMERICS_EXPORT BOOL ludcmp(double **m, int n, int *indx, double *d)
{
    int i, imax, j, k;
    double big, dum, sum, temp;
    double* vv = dvector(0, n - 1);
    
    *d = 1.0;
    for(i = 0; i < n; i++){
        big = 0.0;
        for(j = 0; j < n; j++){
            if((temp = fabs(m[i][j])) > big){
                big = temp;
            }
        }
        if(big == 0.0){
            free_dvector(vv, 0);
            NUMERICS_ERROR("ludcmp Singular Matrix");
            return FALSE;
        }
        vv[i] = 1.0 / big;
    }
    for(j = 0; j < n; j++){
        for(i = 0; i < j; i++){
            sum = m[i][j];
            for(k = 0; k < i; k++){
                sum -= m[i][k] * m[k][j];
            }
            m[i][j] = sum;
        }
        big = 0.0;
        for(i = j; i < n; i++){
            sum = m[i][j];
            for(k = 0; k < j; k++){
                sum -= m[i][k] * m[k][j];
            }
            m[i][j] = sum;
            if((dum = vv[i] * fabs(sum)) >= big){
                big = dum;
                imax = i;
            }
        }
        if(j != imax){
            for(k = 0; k < n; k++){
                dum = m[imax][k];
                m[imax][k] = m[j][k];
                m[j][k] = dum;
            }
            *d = -(*d);
            vv[imax] = vv[j];
        }
        *(indx + j) = imax;
        if(m[j][j] == 0.0){
            m[j][j] = NUMERICS_FLOAT_MIN;
        }
        if(j != n - 1){
            dum = 1.0 / (m[j][j]);
            for(i = j + 1; i < n; i++){
                m[i][j] *= dum;
            }
        }
    }
    
    free_dvector(vv, 0);
    
    return TRUE;
};

NUMERICS_EXPORT void matmat(double **a, int nra, int nca, double **b, int ncb, double **prod)
{
    int i, j, k;
    double sum;
    
    for(i = 0; i < nra; i++){
        for(j = 0; j < ncb; j++){
            sum = 0.0;
            for(k = 0; k < nca; k++){
                sum += a[i][k] * b[k][j];
            }
            prod[i][j] = sum;
        }
    }
}

NUMERICS_EXPORT void matvec(double **a, int nra, int nca, double *x, double *b)
{
    int i, j;
    double sum;
    
    for(i = 0; i < nra; i++){
        sum = 0.0;
        for(j = 0; j < nca; j++){
            sum += a[i][j] * x[j];
        }
        b[i] = sum;
    }
}

NUMERICS_EXPORT void transpose(double **a, int nr, int nc, double **at)
{
    int i, j;
    
    for(i = 0; i < nr; ++i){
        for(j = 0; j < nc; ++j){
            at[j][i] = a[i][j];
        }
    }
}

NUMERICS_EXPORT void vecmat(double *x, double **a, int nra, int nca, double *b)
{
    double** t = dmatrix(0, nca - 1, 0, nra - 1);
    
    transpose(a, nra, nca, t);
    matvec(t, nca, nra, x, b);
    
    free_dmatrix(t, 0, nca - 1, 0);
}

NUMERICS_EXPORT double vecvec(double *first1, double* last1, double* first2)
{
    double p = 1.0;
    
    while(first1 < last1){
        p += *first1 * *first2;
        ++first1;
        ++first2;
    }
    
    return p;
}

NUMERICS_EXPORT double **dmatrix(int nrl, int nrh, int ncl, int nch)
{
    int i;
    double **m;
    
    m = (double **)malloc((size_t)((nrh - nrl + 1)*sizeof(double*)));
    if(!m) return NULL;
    m -= nrl;
    for(i = nrl; i <= nrh; i++){
        m[i] = (double *)malloc((size_t)((nch - ncl + 1)*sizeof(double)));
        if(!(m[i])){
            free_dmatrix(m, nrl, i-1, ncl);
            return NULL;
        }
        m[i] -= ncl;
    }
    return m;
}

NUMERICS_EXPORT void free_dmatrix(double **m, int nrl, int nrh, int ncl)
{
    int i;
    
    for(i = nrh; i >= nrl; i--) free((char*)(m[i]+ncl));
    free((char*)(m+nrl));
}

NUMERICS_EXPORT int **imatrix(int nrl, int nrh, int ncl, int nch)
{
    int i;
    int **m;
    
    m = (int **)malloc((size_t)((nrh - nrl + 1)*sizeof(int*)));
    if(!m) return NULL;
    m -= nrl;
    for(i = nrl; i <= nrh; i++){
        m[i] = (int *)malloc((size_t)((nch - ncl + 1)*sizeof(int)));
        if(!(m[i])){
            free_imatrix(m, nrl, i-1, ncl);
            return NULL;
        }
        m[i] -= ncl;
    }
    return m;
}

NUMERICS_EXPORT void free_imatrix(int **m, int nrl, int nrh, int ncl)
{
    int i;
    
    for(i = nrh; i >= nrl; i--) free((char*)(m[i]+ncl));
    free((char*)(m+nrl));
}


NUMERICS_EXPORT double *dvector(int nl, int nh)
{
    double *v;
    
    v = (double *)malloc((size_t)((nh - nl + 1)*sizeof(double)));
    if(!v) return NULL;
    return v - nl;
}

NUMERICS_EXPORT void free_dvector(double *v, int nl)
{
    free((char *)(v + nl));
}

NUMERICS_EXPORT int *ivector(int nl, int nh)
{
    int *v;
    
    v = (int *)malloc((size_t)((nh - nl + 1)*sizeof(int)));
    if(!v) return NULL;
    return v - nl;
}

NUMERICS_EXPORT void free_ivector(int *v, int nl)
{
    free((char *)(v + nl));
}


NUMERICS_EXPORT void NUMERICS_ERROR(const char *msg)
{
//	mexPrintf(msg);
//    fprintf(stderr, "Numerics Error in routine %s\n", func);
//    fprintf(stderr, "    %s\n", msg);
}

// Blas_ACML.h
// Wrappers for ACML BLAS/LAPACK
// 2007/09/15
//---------------------------------------------------------
#ifndef NDG__umBlas_ACML_H__INCLUDED
#define NDG__umBlas_ACML_H__INCLUDED

// Wrappers for AMD's ACML
#ifdef USE_ACML

//---------------------------------------------------------
// Declare ACML versions of extern C BLAS routines
//---------------------------------------------------------
extern "C" {

  extern double __cdecl dasum(int n, double *x, int incx);
  extern double __cdecl dnrm2(int n, double *x, int incx);
  extern double __cdecl ddot(int n, double *x, int incx, double *y, int incy);
  extern void   __cdecl daxpy(int n, double alpha, double *x, int incx, double *y, int incy);
  extern void   __cdecl dscal(int n, double alpha, double *y, int incy);
         void           dcopy(int n, double *x, int incx, double *y, int incy);

  // General:
  extern void __cdecl dgemm(char transa, char transb, int m, int n, int k, double alpha, double *a, int lda, double *b, int ldb, double beta, double *c, int ldc);
  extern void __cdecl dgemv(char transa, int m, int n, double alpha, double *a, int lda, double *x, int incx, double beta, double *y, int incy);

  // Banded:
  extern void __cdecl dgbmv(char transa, int m, int n, int nsub, int nsuper, double alpha, double *a, int lda, double *x, int incx, double beta, double *y, int incy);
  extern void __cdecl dsbmv(char uplo, int n, int ndiag, double alpha, double *a, int lda, double *x, int incx, double beta, double *y, int incy);


  extern void __cdecl dsyrk(char uplo, char transa, int n, int k, double alpha, double *a, int lda, double beta, double *c, int ldc);

  extern void __cdecl dtrsm(char side, char uplo, char transa, 
    char diag, int m, int n, double alpha, double *a, int lda, 
    double *b, int ldb);


  // LAPACK:
  extern void __cdecl dgesvd(char jobu, char jobvt, int m, int n, double *a, int lda, double *sing, double *u, int ldu, double *vt, int ldvt, int *info);
  extern void __cdecl dgetri(int n, double *a, int lda, int *ipiv, int *info);
  extern void __cdecl dgetrf(int m, int n, double *a, int lda, int *ipiv, int *info);
  extern void __cdecl dgetrs(char transa, int n, int nrhs, double *a, int lda, int *ipiv, double *b, int ldb, int *info);

  extern void __cdecl dgbtrf(int m, int n, int nsub, int nsuper, double *a, int lda, int *ipiv, int *info);
  extern void __cdecl dgbtrs(char transa, int n, int nsub, int nsuper, int nrhs, double *a, int lda, int *ipiv, double *b, int ldb, int *info);
  extern void __cdecl dgelss(int m, int n, int nrhs, double *a, int lda, double *b, int ldb, double *sing, double rcond, int *irank, int *info);

  extern void __cdecl dgesv(int n, int nrhs, double *a, int lda, int *ipiv, double *b, int ldb, int *info);
  extern void __cdecl dposv(char uplo, int n, int nrhs, double *a, int lda, double *b, int ldb, int *info);
  extern void __cdecl dpotrf(char uplo, int n, double *a, int lda, int *info);
  extern void __cdecl dpotrs(char uplo, int n, int nrhs, double *a, int lda, double *b, int ldb, int *info);

  // qr factorization
  extern void __cdecl dgeqrf(int m, int n, double *a, int lda, double *tau, int *info);
  
  extern void __cdecl dpbtrf(char uplo, int n, int ndiag, double *a, int lda, int *info);
  extern void __cdecl dpbtrs(char uplo, int n, int ndiag, int nrhs, double *a, int lda, double *b, int ldb, int *info);
  extern void __cdecl dgeev(char jobvl, char jobvr, int n, double *a, int lda, double *wr, double *wi, double *vl, int ldvl, double *vr, int ldvr, int *info);
  extern void __cdecl dsyev(char jobz, char uplo, int n, double *a, int lda, double *w, int *info);

  extern void __cdecl dgecon(char norm, int n, double *a, int lda, double anorm, double *rcond, int *info);

  typedef struct {
    double real, imag;
  } umDOUBLECOMPLEX;
//extern void __cdecl zgemm(char transa, char transb, int m, int n, int k, std::complex<double> *alpha, std::complex<double> *a, int lda, std::complex<double> *b, int ldb, std::complex<double> *beta, std::complex<double> *c, int ldc);
  extern void __cdecl zgemm(char transa, char transb, int m, int n, int k,      umDOUBLECOMPLEX *alpha,      umDOUBLECOMPLEX *a, int lda,      umDOUBLECOMPLEX *b, int ldb,      umDOUBLECOMPLEX *beta,      umDOUBLECOMPLEX *c, int ldc);

  extern void __cdecl zgecon(char norm, int n, std::complex<double> *a, int lda, double anorm, double *rcond, int *info);

  extern void __cdecl zgetrf(int m, int n, std::complex<double> *a, int lda, int *ipiv, int *info);
  extern void __cdecl zgetri(int n, std::complex<double> *a, int lda, int *ipiv, int *info);
  extern void __cdecl zgetrs(char transa, int n, int nrhs, std::complex<double> *a, int lda, int *ipiv, std::complex<double> *b, int ldb, int *info);

} // extern "C" 


//---------------------------------------------------------
// Define wrappers as required...
//---------------------------------------------------------

// inline double dasum(int n, double *x, int incx);
// inline double dnrm2(int n, double *x, int incx);

inline double ddot(const int& n, const double* x, const int& incx, const double* y, const int& incy)
{
  return ddot(n, (double*)x, incx, (double*)y, incy);
}

// inline void   daxpy(int n, double alpha, double *x, int incx, double *y, int incy);
inline void dcopy(const int& n, const double* x, const int& incx, double* y, const int& incy)
{
  dcopy(n, (double*)x, incx, y, incy);
}

// inline void   dscal(int n, double alpha, double *y, int incy);

// General:
inline void dgemm(const char& transa,const char& transb, const int& m, const int& n, const int& k, 
                  const double& alpha, const double* A, const int& lda, const double* B, const int& ldb, const double& beta, const double* C, const int& ldc)
{
  dgemm(transa, transb, m, n, k, alpha, 
              (double*)A, lda, 
              (double*)B, ldb, beta, 
              (double*)C, ldc);
}

inline void dgemv(const char& trans, const int& m, const int& n, const double& alpha, const double* A, const int& lda, const double* x, const int& incx, const double& beta, const double* y, const int& incy)
{
  dgemv(trans, m, n, alpha, (double*)A, lda, (double*)x, incx, beta, (double*)y, incy);
}

// Banded:
// inline void dgbmv(char transa, int m, int n, int nsub, int nsuper, double alpha, double *a, int lda, double *x, int incx, double beta, double *y, int incy);
// inline void dsbmv(char uplo, int n, int ndiag, double alpha, double *a, int lda, double *x, int incx, double beta, double *y, int incy);
inline void dsbmv(const char& uplo, const int& n, const int& kl, const double& alpha, const double* A, const int& lda, const double* x, const int& incx, const double& beta, const double* y, const int& incy)
{
  char UPLO=uplo; int N=n, KL=kl, LDA=lda, INCX=incx, INCY=incy; 
  double ALPHA=alpha, BETA=beta;
  dsbmv(UPLO, N, KL, ALPHA, (double*)A, LDA, (double*)x, INCX, beta, (double*)y, INCY);
}



// LAPACK:
inline void dgesvd(const char& jobu, const char& jobvt, const int& m, const int& n, const double* A, const int& lda, const double* w, const double* U, const int& ldu, const double* vt, const int& ldvt, const double* work, const int& lwork, int& info)
{
  int INFO=0;
  dgesvd(jobu, jobvt, m, n, (double*)A, lda, (double*)w, (double*)U, ldu, (double*)vt, ldvt, &INFO);
  info=INFO;
}

inline void dgetrf(const int& m, const int& n, const double* A, const int& lda, const int* ipiv, int& info)
{
  int INFO=0;
  dgetrf(m, n, (double*)A, lda, (int*)ipiv, &INFO);
  info=INFO;
}
// inline void dgetri(int n, double *a, int lda, int *ipiv, int *info);
inline void dgetrs(const char& trans,const int& n, const int& nrhs, const double* A, const int& lda, 
                   const int* perm, const double* x, const int& ldx, int& info)
{
  int INFO=0;
  dgetrs(trans, n, nrhs, (double*)A, lda, (int*)perm, (double*)x, ldx, &INFO);
  info=INFO;
}

// inline void dgbtrf(int m, int n, int nsub, int nsuper, double *a, int lda, int *ipiv, int *info);
// inline void dgbtrs(char transa, int n, int nsub, int nsuper, int nrhs, double *a, int lda, int *ipiv, double *b, int ldb, int *info);

inline void dgelss(const int& m, const int& n, const int& nrhs, const double* A, const int& lda, const double* b, const int& ldb, 
                   const double* s, const double& rcond, const int& rank, const double* work, const int& lwork, int& info)
{
  int INFO=0, RANK=rank;
  dgelss(m, n, nrhs, (double*)A, lda, (double*)b, ldb, (double*)s, rcond, &RANK, &INFO);
  info=INFO;
}

inline void dgesv(const int& n, const int& nrhs, double *A, const int& lda, int *ipiv, double *B, const int& ldb, int& info)
{
  int INFO=0;
  dgesv(n, nrhs, A, lda, ipiv, B, ldb, &INFO);
  info=INFO;
}


inline void dposv(const char& uplo, const int& n, const int& nrhs, double *A, const int& lda, double *B, const int& ldb, int& info)
{
  int INFO=0;
  dposv(uplo, n, nrhs, A, lda, B, ldb, &INFO);
  info=INFO;
}

inline void dpotrf(const char& uplo, const int& n, double *A, const int& lda, int& info)
{
  int INFO=0;
  dpotrf(uplo, n, A, lda, &INFO);
  info=INFO;
}

inline void dpotrs(const char& uplo, const int& n, const int& nrhs, const double *A, const int& lda, const double *b, const int& ldb, int& info)
{
  int INFO=0;
  dpotrs(uplo, n, nrhs, (double*)A, lda, (double*)b, ldb, &INFO);
  info=INFO;
}


inline void dgeqrf(const int& m, const int& n, double *a, const int& lda, double *tau, int& info)
{
  int INFO=0;
  dgeqrf(m, n, a, lda, tau, &INFO);
  info=INFO;
}


inline void dpbtrf(const char& uplo, const int& n, const int& kd, double* AB, const int& ldab, int& info)
{
  int INFO=0;
  dpbtrf(uplo, n, kd, AB, ldab, &INFO);
  info=INFO;
}

inline void dpbtrs(const char& uplo, const int& n, const int& kd, const int& nrhs, const double* AB, const int& ldab, double* b, const int& ldb, int& info)
{
  int INFO=0;
  dpbtrs(uplo, n, kd, nrhs, (double*)AB, ldab, b, ldb, &INFO);
  info=INFO;
}

inline void dgeev(const char& jobvl,const char& jobvr,const int& n, const double* a, const int& lda, double* wr, double* wi, double* vl, const int& ldvl, double* vr, const int& ldvr, double* work, const int& lwork, int& info)
{
  int INFO=0;
  dgeev(jobvl, jobvr, n, (double*)a, lda, wr, wi, vl, ldvl, vr, ldvr, &INFO);
  info=INFO;
}

inline void dsyev(const char& jobz, const char& uplo, const int& n, double* a, const int& lda, double* w, double* work, const int& lwork, int& info)
{
  int INFO=0; 
  dsyev(jobz, uplo, n, a, lda, w, &INFO);
  info=INFO;
}

inline void dgecon(const char& NORM, const int& N, double *A, const int& lda, const double& anorm, double& rcond, double *WORK, int *IWORK, int& info)
{
  int INFO=0; 
  double RCOND=rcond;
  dgecon(NORM, N, (double*)A, lda, anorm, &RCOND, &INFO);
  rcond=RCOND;
  info=INFO;
}

inline void zgemm(const char& transa,const char& transb, const int& m, const int& n, const int& k, const std::complex<double>& alpha, const std::complex<double>* A, const int& lda, const std::complex<double>* B, const int& ldb, const std::complex<double>& beta, std::complex<double>* C, const int& ldc)
{
#ifdef USE_ACML

  umERROR("ACML's zgemm", "FIXME: incompatible interface");

  umDOUBLECOMPLEX ALPHA; // = alpha;
  umDOUBLECOMPLEX BETA;  // = beta;
  zgemm(transa, transb, m, n, k, &ALPHA, 
          (umDOUBLECOMPLEX*)A, lda, 
          (umDOUBLECOMPLEX*)B, ldb, &BETA, 
          (umDOUBLECOMPLEX*)C, ldc);
#endif
}


inline void zgecon(const char &norm, const int &N, std::complex<double>* A, const int &ldA, const double &anorm, double &rcond, std::complex<double>* work, double *rwork, int& info)
{
  int INFO=0;
  double RCOND=rcond;
  zgecon(norm, N, A, ldA, anorm, &RCOND, &INFO);
  rcond=RCOND;
  info=INFO;
}

inline void zgetrf(const int &m, const int &n, std::complex<double>* A, const int &ldA, int *ipiv, int& info)
{
  int INFO=0;
  zgetrf(m, n, A, ldA, ipiv, &INFO);
  info=INFO;
}

// inline void zgetri(int n, std::complex<double> *a, int lda, int *ipiv, int *info);
// inline void zgetrs(char transa, int n, int nrhs, std::complex<double> *a, int lda, int *ipiv, std::complex<double> *b, int ldb, int *info);

#endif // USE_ACML
#endif // NDG__umBlas_ACML_H__INCLUDED

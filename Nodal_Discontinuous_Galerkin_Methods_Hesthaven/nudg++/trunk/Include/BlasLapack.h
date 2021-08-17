// BlasLapack.h
// Declare subset of BLAS/LAPACK
// 2007/09/15
//---------------------------------------------------------
#ifndef NDG__umBlasLapack_H__INCLUDED
#define NDG__umBlasLapack_H__INCLUDED

#include <complex>

#ifdef _MSC_VER         // NBN: testing AMD's ACML
#define USE_ACML 1
#else
#undef  USE_ACML
#endif

#ifdef USE_ACML

  // Declare wrappers for ACML versions of BLAS/LAPACK routines
  #include "Blas_ACML.h"
  #undef  UNDERSCORE
  #define NOUNDERSCORE
  #define F77name(X,UX) X

#else

  // NBN: Win32 ATLAS, MKL
  #if defined(WIN32) && !defined(__CYGWIN__)
    #ifndef UNDERSCORE
    #define UNDERSCORE
    #endif
  #elif defined(USE_MKL)
    #ifndef NOUNDERSCORE
    #define NOUNDERSCORE
    #endif
  #endif

  #if defined(UNDERSCORE)
    #define F77name(X,UX) X ## _
  #elif defined(PREUNDERSCORE)
    #define F77name(X,UX) _ ## X ## _
  #elif defined(NOUNDERSCORE)
    #define F77name(X,UX) X
  #endif

extern "C" {

  double F77name(dasum,DASUM)(const int& n, const double* x, const int& incx);
  double F77name(dnrm2,DNRM2)(const int& n, const double* x, const int& incx);
  double F77name(ddot, DDOT )(const int& n, const double* x, const int& incx, const double* y, const int& incy);

  void F77name(daxpy,DAXPY)(const int& n, const double& alpha, const double* x, const int& incx, /*const*/ double* y, const int& incy);
  void F77name(dscal,DSCAL)(const int& n, const double& alpha, /*const*/ double* x, const int& incx);
  void F77name(dcopy,DCOPY)(const int& n, const double* x, const int& incx, /*const*/ double* y, const int& incy);

  // General:
  void F77name(dgemm,DGEMM)(const char& transa,const char& transb, const int& m, const int& n, const int& k, const double& alpha, const double* A, const int& lda, const double* B, const int& ldb, const double& beta, const double* C, const int& ldc);
  void F77name(dgemv,DGEMV)(const char& trans, const int& m, const int& n, const double& alpha, const double* A, const int& lda, const double* x, const int& incx, const double& beta, const double* y, const int& incy);

  // Banded:
  void F77name(dgbmv,DGBMV)(const char& trans, const int& m, const int& n, const int& kl, const int& ku, const double& alpha, const double* A, const int& lda, const double* x, const int& incx, const double& beta, const double* y, const int& incy);
  void F77name(dsbmv,DSBMV)(const char& uplo, const int& n, const int& kl, const double& alpha, const double* A, const int& lda, const double* x, const int& incx, const double& beta, const double* y, const int& incy);

  // Symmetric
  void F77name(dsyrk,DSYRK)(const char& uplo, const char& transa, const int& n, const int& k, const double& alpha, const double *a, const int& lda, const double& beta, double *c, const int& ldc);
  void F77name(dtrsm,DTRSM)(const char& side, const char& uplo, const char& transa, const char& diag, const int& m, const int& n, const double& alpha, double *a, const int& lda, double *b, const int& ldb);

  // LAPACK:
  void F77name(dgesvd,DGESVD)(const char& jobu, const char& jobvt, const int& m, const int& n, const double* A, const int& lda, const double* w, const double* U, const int& ldu, const double* vt, const int& ldvt, const double* work, const int& lwork, int& info);
  void F77name(dgetri,DGETRI)(const int&  n,    const double* A, const int& lda, const int* ipiv, const double* work, const int& lwork, int& info);
  void F77name(dgetrf,DGETRF)(const int&  m,    const int& n, const double* A, const int& lda, const int* ipiv, int& info);
  void F77name(dgetrs,DGETRS)(const char& trans,const int& n, const int& nrhs, const double* A, const int& lda, const int* perm, double* x, const int& ldx, int& info);

  void F77name(dgbtrf,DGBTRF)(const int&  m,    const int& n, const int& kl, const int& ku, const double* A, const int& lda, const int* perm , int& info);
  void F77name(dgbtrs,DGBTRS)(const char& trans,const int& n, const int& kl, const int& ku, const int& nrhs, const double* A, const int& lda, const int* perm, const double* x, const int& ldx, int& info);
  void F77name(dgelss,DGELSS)(const int&  m,    const int& n, const int& nrhs, const double* A, const int& lda, const double* b, const int& ldb, const double* s, const double& rcond, const int& rank, const double* work, const int& lwork, int& info);

  // solve via LU factorization
  void F77name(dgesv, DGESV )(const int&  n,    const int& nrhs, double *A, const int& lda, int *ipiv, double *B, const int& ldb, int& info);
  // solve via Cholesky factorization
  void F77name(dposv, DPOSV )(const char& uplo, const int& n, const int& nrhs, double *A, const int& lda, double *B, const int& ldb, int& info);
  void F77name(dpotrf,DPOTRF)(const char& uplo, const int& n, double *A, const int& lda, int& info);
  void F77name(dpotrs,DPOTRS)(const char& uplo, const int& n, const int& nrhs, double *A, const int& lda, double *b, const int& ldb, int& INFO);

  // qr factorization
  void F77name(dgeqrf,DGEQRF)(const int& m, const int& n, double *a, const int& lda, double *tau, int& info);

  void F77name(dpbtrf,DPBTRF)(const char& uplo, const int& n, const int& kd, double* AB, const int& ldab, int& info);
  void F77name(dpbtrs,DPBTRS)(const char& uplo, const int& n, const int& kd, const int& nrhs, const double* AB, const int& ldab, double* b, const int& ldb, int& info);
  void F77name(dgeev, DGEEV )(const char& jobvl,const char& jobvr,const int& n, const double* a, const int& lda, double* wr, double* wi, double* vl, const int& ldvl, double* vr, const int& ldvr, double* work, const int& lwork, int& info);
  void F77name(dsyev, DSYEV )(const char& jobz, const char& uplo, const int& n, double* a, const int& lda, double* w, double* work, const int& lwork, int& info);
  void F77name(dgecon,DGECON)(const char& NORM, const int& N, double *A, const int& LDA, const double& ANORM, double& RCOND, double *WORK, int *IWORK, int& info);

  void F77name(zgemm, ZGEMM)(const char& transa,const char& transb, const int& m, const int& n, const int& k, const std::complex<double>& alpha, const std::complex<double>* A, const int& lda, const std::complex<double>* B, const int& ldb, const std::complex<double>& beta, std::complex<double>* C, const int& ldc);
  void F77name(zgecon, ZGECON)(const char &norm, const int  &N, std::complex<double>* A, const int &ldA, const double &anorm, double &rcond, std::complex<double>* work, double *rwork, int &info);
  void F77name(zgetrf, ZGETRF)(const int &M, const int &N, std::complex<double>* A, const int &ldA, int *ipiv, int &info); 
}

#endif


//---------------------------------------
// BLAS:
//---------------------------------------
#define ASUM F77name(dasum,DASUM)
#define AXPY F77name(daxpy,DAXPY)
#define SCAL F77name(dscal,DSCAL)
#define COPY F77name(dcopy,DCOPY)
#define NRM2 F77name(dnrm2,DNRM2)
#define DOT  F77name(ddot,DDOT)
#define GEMV F77name(dgemv,DGEMV)
#define GEMM F77name(dgemm,DGEMM)
#define GBMV F77name(dgbmv,DGBMV)
#define SBMV F77name(dsbmv,DSBMV)

#define SYRK F77name(dsyrk,DSYRK)
#define TRSM F77name(dtrsm,DTRSM)

#define ZGEMM F77name(zgemm,ZGEMM)
#define ZGECON F77name(zgecon,ZGECON)
#define ZGETRF F77name(zgetrf,ZGETRF)

//---------------------------------------
// LAPACK:
//---------------------------------------
#define GESVD F77name(dgesvd,DGESVD)
#define GETRF F77name(dgetrf,DGETRF)
#define GETRI F77name(dgetri,DGETRI)
#define GETRS F77name(dgetrs,DGETRS)
#define GBTRF F77name(dgbtrf,DGBTRF)
#define GBTRS F77name(dgbtrs,DGBTRS)
#define GELSS F77name(dgelss,DGELSS)

#define GESV  F77name(dgesv, DGESV)
#define POSV  F77name(dposv, DPOSV)
#define POTRF F77name(dpotrf,DPOTRF)
#define POTRS F77name(dpotrs,DPOTRS)

#define GEQRF F77name(dgeqrf,DGEQRF)

#define PBTRF F77name(dpbtrf,DPBTRF)
#define PBTRS F77name(dpbtrs,DPBTRS)
#define GEEV  F77name(dgeev, DGEEV)
#define SYEV  F77name(dsyev, DSYEV)

#define GECON F77name(dgecon,DGECON)

#endif  // NDG__umBlasLapack_H__INCLUDED

// umMat_COL.cpp
// Lapack wrappers using DMat, DVec
// 2007/05/26
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "Mat_COL.h"

//---------------------------------------------------------
void umAxB(const DMat& A, const DMat& B, DMat& C)
//---------------------------------------------------------
{
  //-------------------------
  // C = A * B
  //-------------------------
  // A = op(A) is (M,K)
  // B = op(B) is (K,N)
  //        C  is (M,N)
  //-------------------------
  int M=A.num_rows(), K=A.num_cols(), N=B.num_cols();
  int LDA=M, LDB=K, LDC=M;
  double one=1.0, zero=0.0;
  if (B.num_rows() != K) { umERROR("umAxB(A,B,C)", "wrong dimensions"); }
  C.resize(M,N);

  GEMM ('N','N',M,N,K, one,A.data(),LDA, 
                           B.data(),LDB, 
                      zero,C.data(),LDC);
}


//---------------------------------------------------------
void umAxB(const ZMat& A, const ZMat& B, ZMat& C)
//---------------------------------------------------------
{
  //-------------------------
  // C = A * B
  //-------------------------
  // A = op(A) is (M,K)
  // B = op(B) is (K,N)
  //        C  is (M,N)
  //-------------------------
  int M=A.num_rows(), K=A.num_cols(), N=B.num_cols();
  int LDA=M, LDB=K, LDC=M;
  std::complex<double> one=1.0, zero=0.0;
  if (B.num_rows() != K) { umERROR("umAxB(A,B,C)", "wrong dimensions"); }
  C.resize(M,N);

  ZGEMM ('N','N',M,N,K, one,A.data(),LDA, 
                           B.data(),LDB, 
                      zero,C.data(),LDC);
}


//---------------------------------------------------------
void umAtransxB(const DMat& A, const DMat& B, DMat& C)
//---------------------------------------------------------
{
  //-------------------------
  // C = A^T * B
  //-------------------------
  // A^T = op(A) is (M,K)
  // B   = op(B) is (K,N)
  //          C  is (M,N)
  //-------------------------
  int M=A.num_cols(), K=A.num_rows(), N=B.num_cols();
  int LDA=K, LDB=K, LDC=M;
  double one=1.0, zero=0.0;
  if (B.num_rows() != K) { umERROR("umAtransxB(A,B,C)", "wrong dimensions"); }
  C.resize(M,N);

  GEMM ('T','N',M,N,K, one,A.data(),LDA, 
                           B.data(),LDB, 
                      zero,C.data(),LDC);
}


//---------------------------------------------------------
void umAxBtrans(const DMat& A, const DMat& B, DMat& C)
//---------------------------------------------------------
{
  //-------------------------
  // C = A * B^T
  //-------------------------
  // A   = op(A) is (M,K)
  // B^T = op(B) is (K,N)
  //          C  is (M,N)
  //-------------------------
  int M=A.num_rows(), K=A.num_cols(), N=B.num_rows();
  int LDA=M, LDB=N, LDC=M;
  double one=1.0, zero=0.0;
  if (B.num_cols() != K) { umERROR("umAxBtrans(A,B,C)", "wrong dimensions"); }
  C.resize(M,N);

  GEMM ('N','T',M,N,K, one,A.data(),LDA, 
                           B.data(),LDB, 
                      zero,C.data(),LDC);
}


///////////////////////////////////////////////////////////
//
// LAPACK drivers
//
///////////////////////////////////////////////////////////


// Forward
DMat umSOLVE_LS(const DMat& mat, const DMat& B);


// DGESV computes the solution to a real system of linear 
// equations, A*x = b, where A is an N-by-N matrix, and 
// x and b are N-by-1 vectors.  The LU decomposition 
// with partial pivoting and row interchanges is used to 
// factor A as A = P*L*U, where P is a permutation matrix,
// L is unit lower triangular, and U is upper triangular.
// The system is solved using this factored form of A.
//---------------------------------------------------------
void umSOLVE(const DMat& mat, const DVec& b, DVec& x)
//---------------------------------------------------------
{
  // Work with copies of input arrays.
  DMat A(mat);
  x = b;

  int NRHS = 1;
  int LDA  = A.num_rows();
  int rows = A.num_rows();
  int cols = A.num_cols();
  int info = 0;
  
  if (rows != cols) {
    umERROR("umSOLVE(DMat, DVec)", 
            "Matrix A (%d,%d) is not square.\n"
            "For a Least-Squares solution, see umSOLVE_LS(A,B).", 
            rows, cols);
  }

  if (rows < 1) {
    umLOG(1, "Empty system passed into umSOLVE().\n");
    return;
  }

  IVec ipiv(rows, 0);

  GESV (rows, NRHS, A.data(), LDA, ipiv.data(), x.data(), rows, info);

  if (info < 0) { 
    x = 0.0;
    umERROR("umSOLVE(DMat&, DVec&)", 
            "Error in input argument (%d)\nNo solution computed.", -info);
  } else if (info > 0) {
    x = 0.0;
    umERROR("umSOLVE(DMat&, DVec&)", 
            "\nINFO = %d.  U(%d,%d) was exactly zero."
            "\nThe factorization has been completed, but the factor U is "
            "\nexactly singular, so the solution could not be computed.", 
              info, info, info);
  }
}


// DGESV uses the LU factorization to compute solution 
// to a real system of linear equations, A * X = B, 
// where A is square (N,N) and X, B are (N,NRHS).
//
// If the system is over or under-determined, 
// (i.e. A is not square), then pass the problem
// to the Least-squares solver (DGELSS) below.
//---------------------------------------------------------
void umSOLVE(const DMat& mat, const DMat& B, DMat& X)
//---------------------------------------------------------
{
  if (!mat.ok()) {umWARNING("umSOLVE()", "system is empty"); return;}
  if (!mat.is_square()) {
    umSOLVE_LS(mat, B, X);    // return a least-squares solution.
    return;
  }

  DMat A(mat);    // work with copy of input
  X = B;          // initialize result with RHS

  int rows=A.num_rows(), LDA=A.num_rows(), cols=A.num_cols();
  int LDB=B.num_rows(), NRHS=B.num_cols(), info=0;
  if (rows<1) {umWARNING("umSOLVE()", "system is empty"); return;}
  IVec ipiv(rows);

  // Solve the system.
  GESV(rows, NRHS, A.data(), LDA, ipiv.data(), X.data(), LDB, info);

  if (info < 0) { 
    X = 0.0;
    umERROR("umSOLVE(A,B, X)", 
            "Error in input argument (%d)\nNo solution computed.", -info);
  } else if (info > 0) {
    X = 0.0;
    umERROR("umSOLVE(A,B, X)", 
            "\nINFO = %d.  U(%d,%d) was exactly zero."
            "\nThe factorization has been completed, but the factor U is "
            "\nexactly singular, so the solution could not be computed.", 
              info, info, info);
  }
}


// DPOSV uses Cholesky factorization A=U^T*U, A=L*L^T 
// to compute the solution to a real system of linear 
// equations A*X=B, where A is a square, (N,N) symmetric 
// positive definite matrix and X and B are (N,NRHS).
//---------------------------------------------------------
void umSOLVE_CH(const DMat& mat, const DVec& b, DVec& x)
//---------------------------------------------------------
{
  // check args
  assert(mat.is_square());            // symmetric
  assert(b.size() >= mat.num_rows()); // is b consistent?
  assert(b.size() <= x.size());       // can x store solution?
  
  DMat A(mat);    // work with copy of input
  x = b;          // allocate solution vector

  int rows=A.num_rows(), LDA=A.num_rows(), cols=A.num_cols();
  int  LDB=b.size(), NRHS=1, info=0;
  if (rows<1) {umWARNING("umSOLVE_CH()", "system is empty"); return;}

  // Solve the system.
  POSV('U', rows, NRHS, A.data(), LDA, x.data(), LDB, info);

  if (info < 0) { 
    x = 0.0;
    umERROR("umSOLVE_CH(A,b, x)", 
            "Error in input argument (%d)\nNo solution computed.", -info);
  } else if (info > 0) {
    x = 0.0;
    umERROR("umSOLVE_CH(A,b, x)", 
            "\nINFO = %d.  The leading minor of order %d of A"
            "\nis not positive definite, so the factorization" 
            "\ncould not be completed. No solution computed.", 
              info, info);
  }
}


// DPOSV uses Cholesky factorization A=U^T*U, A=L*L^T 
// to compute the solution to a real system of linear 
// equations A*X=B, where A is a square, (N,N) symmetric 
// positive definite matrix and X and B are (N,NRHS).
//
// If the system is over or under-determined, 
// (i.e. A is not square), then pass the problem
// to the Least-squares solver (DGELSS) below.
//---------------------------------------------------------
void umSOLVE_CH(const DMat& mat, const DMat& B, DMat& X)
//---------------------------------------------------------
{
  if (!mat.ok()) {umWARNING("umSOLVE_CH()", "system is empty"); return;}
  if (!mat.is_square()) {
    umSOLVE_LS(mat, B, X);    // return a least-squares solution.
    return;
  }
  
  DMat A(mat);    // Work with a copy of input array.
  X = B;          // initialize solution with rhs

  int rows=A.num_rows(), LDA=A.num_rows(), cols=A.num_cols();
  int LDB=X.num_rows(), NRHS=X.num_cols(), info=0;
  assert(LDB >= rows);  // enough space for solutions?

  // Solve the system.
  POSV('U', rows, NRHS, A.data(), LDA, X.data(), LDB, info);

  if (info < 0) { 
    X = 0.0;
    umERROR("umSOLVE_CH(A,B, X)", 
            "Error in input argument (%d)\nNo solution computed.", -info);
  } else if (info > 0) {
    X = 0.0;
    umERROR("umSOLVE_CH(A,B, X)", 
            "\nINFO = %d.  The leading minor of order %d of A"
            "\nis not positive definite, so the factorization" 
            "\ncould not be completed. No solution computed.", 
              info, info);
  }
}


// DGELSS computes minimum norm solution to a real linear 
// least squares problem:   Minimize 2-norm(| b - A*x |).   
// using the singular value decomposition (SVD) of A. 
// A is an M-by-N matrix which may be rank-deficient.   
//---------------------------------------------------------
void umSOLVE_LS(const DMat& mat, const DMat& B, DMat& X)
//---------------------------------------------------------
{
  if (!mat.ok()) {umWARNING("umSOLVE_LS()", "system is empty"); return;}

  DMat A(mat);    // work with copy of input.

  int rows=A.num_rows(), cols=A.num_cols(), mmn=A.min_mn();
  int LDB=A.max_mn(), NRHS=B.num_cols();
  if (rows!=B.num_rows()) {umERROR("umSOLVE_LS(A,B)", "Inconsistant matrix sizes.");}

  DVec s(mmn);    // allocate array for singular values

  // X must be big enough to store various results.
  // Resize X so that its leading dimension = max(M,N), 
  // then load the set of right hand sides.

  X.resize(LDB,NRHS, true, 0.0);

  for (int j=1; j<=NRHS; ++j)     // loop across colums
    for (int i=1; i<=rows; ++i)   // loop down rows
      X(i,j) = B(i,j);

  // RCOND is used to determine the effective rank of A.   
  // Singular values S(i) <= RCOND*S(1) are treated as zero.   
  // If RCOND < 0, machine precision is used instead.   

//double rcond =  1.0 / 1.0e16;
  double rcond = -1.0;

  // NBN: ACML does not use the work vector.
  int mnLo=A.min_mn(), mnHi=A.max_mn(), rank=1, info=1;
  int lwork = 10*mnLo + std::max(2*mnLo, std::max(mnHi, NRHS));
  DVec work(lwork); 

  // Solve the system
  GELSS (rows, cols, NRHS, A.data(), rows, X.data(), LDB, s.data(), rcond, rank, work.data(), lwork, info);

  //---------------------------------------------
  // Report:
  //---------------------------------------------

  if (info == 0) {
    umLOG(1, "umSOLVE_LS reports successful LS-solution."
             "\nRCOND = %0.6e, "
             "\nOptimal length of work array was %d\n", rcond, lwork);
  } 
  else 
  {
    if (info < 0) { 
      X = 0.0;
      umERROR("umSOLVE_LS(DMat&, DMat&)", 
              "Error in input argument (%d)\nNo solution or error bounds computed.", -info);

    } else if (info > 0) {
      X = 0.0;
      umERROR("umSOLVE_LS(DMat&, DMat&)", 
          "\nThe algorithm for computing the SVD failed to converge.\n"
          "\n%d off-diagonal elements of an intermediate "
          "\nbidiagonal form did not converge to zero.\n "
          "\nRCOND = %0.6e, "
          "\nOptimal length of work array was %d.\n", info, rcond, lwork);
    }
  }
}



///////////////////////////////////////////////////////////
//
// REAL version: svd(DMat&)
//
///////////////////////////////////////////////////////////


// Computes an SVD factorization of a real MxN matrix.
// Returns the vector of singular values.
// Also, factors U, VT, where A = U * D * VT.
//---------------------------------------------------------
DVec& svd
(
  const DMat& mat,  // [in]
        DMat& U,    // [out: left singular vectors]
        DMat& VT,   // [out: right singular vectors]
        char ju,    // [in: want U?]
        char jvt    // [in: want VT?]
)
//---------------------------------------------------------
{
  // Work with a copy of the input matrix.
  DMat A(mat, OBJ_temp, "svd.TMP");

  // A(MxN)
  int m=A.num_rows(), n=A.num_cols();
  int mmn=A.min_mn(), xmn=A.max_mn();

  // resize parameters
  U.resize (m,m, true, 0.0);
  VT.resize(n,n, true, 0.0);
  DVec* s = new DVec(mmn, 0.0, OBJ_temp, "s.TMP");
  char jobu  = ju;
  char jobvt = jvt;
  int info = 0;

  // NBN: ACML does not use the work vector.
  int lwork = 2 * std::max(3*mmn+xmn, 5*mmn);
  DVec work(lwork, 0.0, OBJ_temp, "work.TMP");
  GESVD (jobu, jobvt, m, n, A.data(), m, s->data(), U.data(), m, VT.data(), n, work.data(), lwork, info);

  if (info < 0) { 
    (*s) = 0.0;
    umERROR("SVD", "Error in input argument (%d)\nNo solution computed.", -info);
  } else if (info > 0) {
    (*s) = 0.0;
    umLOG(1, "DBDSQR did not converge."
             "\n%d superdiagonals of an intermediate bidiagonal"
             "\nform B did not converge to zero.\n", info);
  }

  return (*s);
}



// Overloaded version:  Return singular values only.
//---------------------------------------------------------
DVec& svd(const DMat& mat) 
//---------------------------------------------------------
{
  DMat U, VT;  
  return svd(mat, U, VT, 'N', 'N'); 
}


// Norm: Return largest singular value for M-by-N matrix.
//---------------------------------------------------------
double norm(const DMat& mat) 
//---------------------------------------------------------
{
  DVec s( svd(mat) );
  return s(1); 
}


// Condition estimate: 
// Return ratio of largest/smallest singular values.
//---------------------------------------------------------
double cond(const DMat& mat)
//---------------------------------------------------------
{
  DVec s( svd(mat) );
  int n = s.size();
  if (fabs(s(n)) < DBL_MIN)
  {
    umERROR("Condition estimate via SVD", 
            "Division by zero singular value");
    s(n) = DBL_MIN;
  }

  return s(1) / s(n);
}


// Condition estimate: 
// Return ratio of largest/smallest singular values.
//---------------------------------------------------------
double cond(ZMat& mat)
//---------------------------------------------------------
{
  double rcond = 1.;

  ZVec  work(2*mat.num_cols(), "work");
  DVec rwork(2*mat.num_cols(), "rwork");

  int info;

  ZMat matcopy(mat);
  IVec ipiv(mat.num_rows(), "ipiv");

  ZGETRF(mat.num_rows(),
         mat.num_cols(),
         matcopy.data(),
         mat.num_rows(),
         ipiv.data(),
         info);


  //  fprintf(stdout, "zgetrf: info=%d \n", info);

  // must fix this
  double anorm = 1.0;

  ZGECON('I',
          mat.num_cols(), 
          matcopy.data(), 
          mat.num_rows(),
          anorm, 
          rcond, 
          work.data(), 
          rwork.data(), 
          info);

  //  fprintf(stdout, "zgecon: info=%d rcond=%lf\n", info, rcond);

  return 1./rcond;
}


//---------------------------------------------------------
void eig(const DMat& A, DVec& Re)
//---------------------------------------------------------
{
  // Compute eigenvalues of a real general matrix
  // Currently NOT returning imaginary components
  DMat VL("VL"), VR("VR");
  eig(A, Re, VL, VR, false, false);
}


//---------------------------------------------------------
void eig(const DMat& A, DVec& Re, DMat& VR)
//---------------------------------------------------------
{
  // Compute eigenvalues and RIGHT eigenvectors of a real 
  // general matrix.  NOT returning imaginary components.
  DMat VL("VL");
  eig(A, Re, VL, VR, false, true);
}

//---------------------------------------------------------
void eig(const DMat& A, DVec& Re, DMat& VL, DMat& VR, bool bL, bool bR)
//---------------------------------------------------------
{
  // Compute eigensystem of a real general matrix
  // Currently NOT returning imaginary components

  static DMat B;

  if (!A.is_square()) { umERROR("eig(A)", "matrix is not square."); }

  int N = A.num_rows();
  int LDA=N, LDVL=N, LDVR=N, ldwork=10*N, info=0;

  Re.resize(N);     // store REAL components of eigenvalues in Re
  VL.resize(N,N);   // storage for LEFT eigenvectors
  VR.resize(N,N);   // storage for RIGHT eigenvectors
  DVec Im(N);     // NOT returning imaginary components
  DVec work(ldwork, 0.0);

  // Work on a copy of A
  B = A;

  char jobL = bL ? 'V' : 'N';   // calc LEFT eigenvectors?
  char jobR = bR ? 'V' : 'N';   // calc RIGHT eigenvectors?

  GEEV (jobL,jobR, N, B.data(), LDA, Re.data(), Im.data(), 
        VL.data(), LDVL, VR.data(), LDVR, work.data(), ldwork, info);

  if (info < 0) { 
    umERROR("eig(A, Re,Im)", "Error in input argument (%d)\nNo solution computed.", -info);
  } else if (info > 0) {
    umLOG(1, "eig(A, Re,Im): ...\n"
             "\nThe QR algorithm failed to compute all the"
             "\neigenvalues, and no eigenvectors have been" 
             "\ncomputed;  elements %d+1:N of WR and WI contain"
             "\neigenvalues which have converged.\n", info);
  }

#if (0)
  // Return (Re,Imag) parts of eigenvalues as columns of Ev
  Ev.resize(N,2);
  Ev.set_col(1, Re);
  Ev.set_col(2, Im);
#endif

#ifdef _DEBUG
    //#####################################################
    // Check for imaginary components in eigenvalues
    //#####################################################
    double im_max = Im.max_val_abs();
    if (im_max > 1e-6) {
      umERROR("eig(A)", "imaginary components in eigenvalues.");
    }
    //#####################################################
#endif
}


// compute eigensystem of a real symmetric matrix
//---------------------------------------------------------
void eig_sym(const DMat& A, DVec& ev, DMat& Q, bool bDoEVecs)
//---------------------------------------------------------
{
  if (!A.is_square()) { umERROR("eig_sym(A)", "matrix is not square."); }

  int N = A.num_rows();
  int LDA=N, LDVL=N, LDVR=N, ldwork=10*N, info=0;
  DVec work(ldwork, 0.0, OBJ_temp, "work_TMP");

  Q = A;          // Calculate eigenvectors in Q (optional)
  ev.resize(N);   // Calculate eigenvalues in ev

  char jobV = bDoEVecs ? 'V' : 'N';

  SYEV (jobV,'U', N, Q.data(), LDA, ev.data(), work.data(), ldwork, info);  

  if (info < 0) { 
    umERROR("eig_sym(A, Re,Im)", "Error in input argument (%d)\nNo solution computed.", -info);
  } else if (info > 0) {
    umLOG(1, "eig_sym(A, W): ...\n"
             "\nthe algorithm failed to converge;"
             "\n%d off-diagonal elements of an intermediate"
             "\ntridiagonal form did not converge to zero.\n", info);
  }
}


//---------------------------------------------------------
DMat& lu(DMat& A, bool in_place)
//---------------------------------------------------------
{
  // Given square matrix A, return its lu-factorization 
  // for use later in solving (multiple) linear systems.

  if (!A.is_square()) { umERROR("lu(A)", "matrix is not square."); }
  int rows=A.num_rows(); int N=rows, LDA=rows, info=0;
  int* ipiv = umIVector(rows);

  if (in_place) 
  {
    // factorize arg
    GETRF(N, N, A.data(), LDA, ipiv, info);
    if (info) { umERROR("lu(A)", "dgetrf reports: info = %d", info); }
    A.set_pivots(ipiv);        // store pivots
    A.set_factmode(FACT_LUP);  // indicate factored state
    return A;
  } 
  else
  {
    // factorize copy of arg
    DMat* tmp = new DMat(A, OBJ_temp, "lu(A)");
    GETRF(N, N, tmp->data(), LDA, ipiv, info);
    if (info) { umERROR("lu(A)", "dgetrf reports: info = %d", info); }
    tmp->set_pivots(ipiv);        // store pivots
    tmp->set_factmode(FACT_LUP);  // indicate factored state
    return (*tmp);
  }
}


//---------------------------------------------------------
bool lu_solve(DMat& LU, const DMat& B, DMat& X)
//---------------------------------------------------------
{
  // Solve a set of linear systems using lu-factored square matrix.
  try {
    LU.solve_LU(B, X, false, false);
  } catch(...) { return false; }
  return true;
}


//---------------------------------------------------------
DVec& lu_solve(DMat& LU, const DVec& b)
//---------------------------------------------------------
{
  // Solve a linear system using lu-factored square matrix.

  DVec *x = new DVec("x", OBJ_temp);
  try {
    LU.solve_LU(b, (*x), false, false);
  } catch(...) { x->Free(); }
  return (*x);
}


// overload to allow Region1D as rhs arg
//---------------------------------------------------------
DVec& lu_solve(DMat& LU, Region1D<DVec> R)
//---------------------------------------------------------
{
  DVec rhs(R);
  return lu_solve(LU,rhs);
}


//---------------------------------------------------------
DMat& chol(DMat& A, bool in_place)
//---------------------------------------------------------
{
  // Given symmetric positive-definite matrix A,
  // return its Cholesky-factorization for use
  // later in solving (multiple) linear systems.

  int M=A.num_rows(), LDA=A.num_rows(), info=0;
  char uplo = 'U';

  if (in_place) 
  {
    // factorize arg
    POTRF (uplo, M, A.data(), LDA, info);
    if (info) { umERROR("chol(A)", "dpotrf reports: info = %d", info); }
    A.zero_below_diag();
    A.set_factmode(FACT_CHOL);  // indicate factored state
    return A;
  } 
  else
  {
    // factorize copy of arg
    DMat* tmp = new DMat(A, OBJ_temp, "chol(A)");
    POTRF (uplo, M, tmp->data(), LDA, info);
    if (info) { umERROR("chol(A)", "dpotrf reports: info = %d", info); }
    tmp->zero_below_diag();
    tmp->set_factmode(FACT_CHOL);  // indicate factored state
#if (0)
    // compare with Matlab
    tmp->print(g_MSGFile, "chol", "lf", 4, 8);
#endif
    return (*tmp);
  }
}


//---------------------------------------------------------
bool chol_solve(const DMat& ch, const DMat& B, DMat& X)
//---------------------------------------------------------
{
  // Solve a set of linear systems using Cholesky-factored 
  // symmetric positive-definite matrix, A = U^T U.

  if (FACT_CHOL != ch.get_factmode()) {umERROR("chol_solve(ch,B,X)", "matrix is not factored.");}
  int M =ch.num_rows(), lda=ch.num_rows(); 
  int ldb=B.num_rows(), nrhs=B.num_cols(); assert(ldb == M);
  char uplo = 'U';  int info=0; 
  double* ch_data = const_cast<double*>(ch.data());

  X = B;  // overwrite X with RHS's, then solutions
  POTRS (uplo, M, nrhs, ch_data, lda, X.data(), ldb, info);

  if (info) { umERROR("chol_solve(ch,B,X)", "dpotrs reports: info = %d", info); }
  return true;
}


//---------------------------------------------------------
DVec& chol_solve(const DMat& ch, const DVec& b)
//---------------------------------------------------------
{
  // Solves a linear system using Cholesky-factored 
  // symmetric positive-definite matrix, A = U^T U.

  if (FACT_CHOL != ch.get_factmode()) {umERROR("chol_solve(ch,b)", "matrix is not factored.");}
  int M=ch.num_rows(), lda=ch.num_rows(); 
  int nrhs=1, ldb=b.size();   assert(ldb == M);
  char uplo = 'U';  int info=0; 
  double* ch_data = const_cast<double*>(ch.data());

  // copy RHS into x, then overwrite x with solution
  DVec* x = new DVec(b, OBJ_temp);
  POTRS (uplo, M, nrhs, ch_data, lda, x->data(), ldb, info);
  if (info) { umERROR("chol_solve(ch,b)", "dpotrs reports: info = %d", info); }
  return (*x);
}


// overload to allow Region1D as rhs arg
//---------------------------------------------------------
DVec& chol_solve(const DMat& ch, Region1D<DVec> R)
//---------------------------------------------------------
{
  DVec rhs(R);
  return chol_solve(ch,rhs);
}


//---------------------------------------------------------
DMat& qr(DMat& A, bool in_place)
//---------------------------------------------------------
{
  // Form orthogonal QR factorization of A(m,n). 
  // The result Q is represented as a product of 
  // min(m, n) elementary reflectors. 

  int M=A.num_rows(), N=A.num_cols(), LDA=A.num_rows();
  int min_mn = A.min_mn(), info=0; DVec tau(min_mn);

  if (in_place) 
  {
    // factorize arg
    GEQRF(M, N, A.data(), LDA, tau.data(), info);

    if (info) { umERROR("qr(A)", "dgeqrf reports: info = %d", info); }
  //A.set_qrtau(tau);         // H(i) = I - tau * v * v'
    A.set_factmode(FACT_QR);  // indicate factored state
    return A;
  } 
  else
  {
    // factorize copy of arg
    DMat* tmp = new DMat(A, OBJ_temp, "qr(A)");
    GEQRF (M, N, tmp->data(), LDA, tau.data(), info);

    if (info) { umERROR("qr(A)", "dgeqrf reports: info = %d", info); }
  //tmp->set_qrtau(tau);         // H(i) = I - tau * v * v'
    tmp->set_factmode(FACT_QR);  // indicate factored state
    return (*tmp);
  }
}



//---------------------------------------------------------
void umPOLISH(DVec& V, double eps)
//---------------------------------------------------------
{
  // round elements close to certain values

  int N = V.size();
  double *p = V.data();

  for (int i=0; i<N; ++i) 
  {
    if (fabs(p[i]) < eps) 
    {
      p[i] = 0.0;
    }
    else
    {
      if (p[i] > 0.0) 
      {
        // check for proximity to certain positive values
        if      (fabs (p[i] - 0.10) < eps) { p[i] = 0.10; }
        else if (fabs (p[i] - 0.20) < eps) { p[i] = 0.20; }
        else if (fabs (p[i] - 0.25) < eps) { p[i] = 0.25; }
        else if (fabs (p[i] - 0.50) < eps) { p[i] = 0.50; }
        else if (fabs (p[i] - 0.75) < eps) { p[i] = 0.75; }
        else if (fabs (p[i] - 0.80) < eps) { p[i] = 0.80; }
        else if (fabs (p[i] - 0.90) < eps) { p[i] = 0.90; }
        else if (fabs (p[i] - 1.00) < eps) { p[i] = 1.00; }
        else if (fabs (p[i] - 2.00) < eps) { p[i] = 2.00; }
        else if (fabs (p[i] - 4.00) < eps) { p[i] = 4.00; }
        else if (fabs (p[i] - 4.50) < eps) { p[i] = 4.50; }
        else if (fabs (p[i] - 5.00) < eps) { p[i] = 5.00; }

        else if (fabs (p[i] - M_PI  ) < eps) { p[i] = M_PI  ; }
        else if (fabs (p[i] - M_PI_2) < eps) { p[i] = M_PI_2; }
        else if (fabs (p[i] - M_PI_4) < eps) { p[i] = M_PI_4; }
        else if (fabs (p[i] - M_E   ) < eps) { p[i] = M_E   ; }
      }
      else
      {
        // check for proximity to certain negative values
        if      (fabs (p[i] + 0.10) < eps) { p[i] = -0.10; }
        else if (fabs (p[i] + 0.20) < eps) { p[i] = -0.20; }
        else if (fabs (p[i] + 0.25) < eps) { p[i] = -0.25; }
        else if (fabs (p[i] + 0.50) < eps) { p[i] = -0.50; }
        else if (fabs (p[i] + 0.75) < eps) { p[i] = -0.75; }
        else if (fabs (p[i] + 0.80) < eps) { p[i] = -0.80; }
        else if (fabs (p[i] + 0.90) < eps) { p[i] = -0.90; }
        else if (fabs (p[i] + 1.00) < eps) { p[i] = -1.00; }
        else if (fabs (p[i] + 2.00) < eps) { p[i] = -2.00; }
        else if (fabs (p[i] + 4.00) < eps) { p[i] = -4.00; }
        else if (fabs (p[i] + 4.50) < eps) { p[i] = -4.50; }
        else if (fabs (p[i] + 5.00) < eps) { p[i] = -5.00; }

        else if (fabs (p[i] + M_PI  ) < eps) { p[i] = -M_PI  ; }
        else if (fabs (p[i] + M_PI_2) < eps) { p[i] = -M_PI_2; }
        else if (fabs (p[i] + M_PI_4) < eps) { p[i] = -M_PI_4; }
        else if (fabs (p[i] + M_E   ) < eps) { p[i] = -M_E   ; }
      }
    }
  }
}


//---------------------------------------------------------
void umPOLISH(DMat& A, double eps)
//---------------------------------------------------------
{
  DVec& V = dynamic_cast<DVec&>(A);
  umPOLISH(V, eps);
}


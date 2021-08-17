// CS_Solve.cpp
// Implementation of various sparse solvers using class CSd
// 2007/10/16
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CS_Type.h"

#include "stopwatch.h"


///////////////////////////////////////////////////////////
//
// class CSS (CS Symbolic)
//
///////////////////////////////////////////////////////////

//---------------------------------------------------------
CSS::CSS() 
//---------------------------------------------------------
  : pinv("CSS.pinv"), Q("CSS.Q"), parent("CSS.parent"),
    cp("CSS.cp"), leftmost("leftmost"), m2(0),
    lnz(0.0),unz(0.0),m_mode(OBJ_real) 
{}


//---------------------------------------------------------
CSS::~CSS()
//---------------------------------------------------------
{
  Free(); 
}


//---------------------------------------------------------
void CSS::Free()
//---------------------------------------------------------
{
  // force deallocation of arrays from registry
  pinv.Free(); Q.Free(); parent.Free();
  cp.Free(); leftmost.Free();
}


//---------------------------------------------------------
void CSS::show_alloc() const
//---------------------------------------------------------
{
  umMSG(1, "\nAllocations in Symbolic object:\n");
  umMSG(1, "   pinv  : %8d (int) \n", pinv.size());
  umMSG(1, "   Q     : %8d (int) \n", Q.size());
  umMSG(1, "   parent: %8d (int) \n", parent.size());
  umMSG(1, "   cp    : %8d (int) \n", cp.size());
  umMSG(1, " leftmost: %8d (int) \n", leftmost.size());
  umMSG(1, " m2: %d  lnz: %g  unz: %g \n\n", m2, lnz, unz);
}


//---------------------------------------------------------
bool CSS::ok() const 
//---------------------------------------------------------
{ 
  if (!cp.ok())     { return false; } // check column pointers
  if (!parent.ok()) { return false; } // check elimination tree
  // ...
  return true; 
}



///////////////////////////////////////////////////////////
//
// class CSN (CS Numeric)
//
///////////////////////////////////////////////////////////

//---------------------------------------------------------
CSN::CSN() 
//---------------------------------------------------------
  : L("CSN.L"),U("CSN.U"),C(NULL),E(NULL),m_mode(OBJ_real) 
{}


//---------------------------------------------------------
CSN::~CSN()
//---------------------------------------------------------
{
  Free(); 
}


//---------------------------------------------------------
void CSN::Free()
//---------------------------------------------------------
{
  // force deallocation of arrays from registry
  L.reset(); U.reset(); pinv.Free(); B.Free();
}


//---------------------------------------------------------
void CSN::show_alloc() const
//---------------------------------------------------------
{
  umMSG(1, "\nAllocations in Numeric object:\n");
  umMSG(1, "   L     : %8d (csc) \n", L.size());
  umMSG(1, "   U     : %8d (csc) \n", U.size());
  umMSG(1, "   pinv  : %8d (int) \n", pinv.size());
  umMSG(1, "   B     : %8d (dbl) \n\n", B.size());
}


//---------------------------------------------------------
bool CSN::ok() const 
//---------------------------------------------------------
{ 
  if (!L.ok()) return false;
//if (!U.ok()) return false;
//if (!pinv.ok()) return false;
//if (!B.ok()) return false;
  return true; 
}



///////////////////////////////////////////////////////////
//
// Cholesky factorization
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
CS_Chol::CS_Chol()
//---------------------------------------------------------
  : S(NULL), N(NULL)
{
}

//---------------------------------------------------------
CS_Chol::~CS_Chol()
//---------------------------------------------------------
{
  if (S) { delete S; S=NULL; }
  if (N) { delete N; N=NULL; }
}


//---------------------------------------------------------
int CS_Chol::chol(CSd& A, int order, double dummy)
//---------------------------------------------------------
{
  // Perform Cholesky factorization using 
  // appropriate AMD re-ordering mode:
  //---------------------------------------
  // 0: natural: C = A     (no reordering)
  // 1: Chol   : C = A+A'
  // 2: LU     : C = A'*A  (drop dense rows)
  // 3: QR     : C = A'*A
  // 4: Chol#2 : C = A     (A is symmetric)
  //---------------------------------------

  // clear existing system
  if (S) { delete S; S = NULL; }
  if (N) { delete N; N = NULL; }

  // check matrix input
  if (!A.ok())        {umERROR("CS_Chol::chol", "empty matrix"); return 0;}
  if (!A.is_csc())    {umERROR("CS_Chol::chol", "expected csc form"); return 0;}
  if (!A.is_square()) {umERROR("CS_Chol::chol", "matrix must be square"); return 0;}

  umLOG(1, "\nCS_Chol:chol -- starting symbolic phase\n");
  try {
    // ordering and symbolic analysis
    S = CS_schol(order, A);
    if (!S) { umERROR("CS_Chol::chol", "error building symbolic info"); return -1;}
  } catch(...) {
    umERROR("CS_Chol:chol", "exception in symbolic phase"); return -1;
  }
  umLOG(1, "CS_Chol:chol -- symbolic phase complete\n");
  umLOG(1, "CS_Chol:chol -- size of full Cholesky L = %1.0lf\n\n", S->lnz);
  try {
    // numeric Cholesky factorization
    N = CS_chol(A, S, true);  // take ownership of A's data
    if (!N) { umERROR("CS_Chol::chol", "error building numeric data"); return -2;}
  } catch(...) {
    umERROR("CS_Chol:chol", "exception in numeric phase"); return -2;
  }

  return 1;
}


//---------------------------------------------------------
DVec& CS_Chol::solve(const DVec& rhs)
//---------------------------------------------------------
{
  // use factored form to solve for rhs, return x=A\rhs

  // check {symbolic, numeric} data is ready
  if (!S || !N) {umERROR("CS_Chol::solve", "system not factorized"); return x;}

  // allocate arrays
  int n=rhs.size(); b=rhs; x.resize(n);
  if (!b.ok()||!x.ok()) {umERROR("CS_Chol::solve", "out of memory"); return x;}

  CS_ipvec  (S->pinv, b, x, n); // x = P*b
  CS_lsolve (N->L,    x);       // x = L\x
  CS_ltsolve(N->L,    x);       // x = L'\x
  CS_pvec   (S->pinv, x, b, n); // b = P'*x
  return b; 
}


//---------------------------------------------------------
DVec& CS_Chol::chol_solve(int order, CSd& A, DVec& rhs) 
//---------------------------------------------------------
{
  // factor and solve for rhs, return x=A\rhs
  assert(A.ok() && A.is_csc() && A.is_square());
  int n=A.n; b=rhs; x.resize(n);
  if (!x.ok()||!b.ok()) { umERROR("CS_Chol::chol_solve", "out of memory"); }

  S = CS_schol(order, A);         // ordering and symbolic analysis
  N = CS_chol(A, S);              // numeric Cholesky factorization
  if (!S || !N) { umERROR("CS_Chol::chol_solve", "setup failed"); }

  CS_ipvec  (S->pinv, b, x, n); // x = P*b
  CS_lsolve (N->L,    x);       // x = L\x
  CS_ltsolve(N->L,    x);       // x = L'\x
  CS_pvec   (S->pinv, x, b, n); // b = P'*x
  delete S; delete N;
  return b; 
}


///////////////////////////////////////////////////////////
//
// LU factorization
//
///////////////////////////////////////////////////////////

//---------------------------------------------------------
CS_LU::CS_LU()
//---------------------------------------------------------
 : S(NULL), N(NULL)
{
}

//---------------------------------------------------------
CS_LU::~CS_LU()
//---------------------------------------------------------
{
  if (S) { delete S; S=NULL; }
  if (N) { delete N; N=NULL; }
}


//---------------------------------------------------------
int CS_LU::lu(const CSd& A, int order, double tol)
//---------------------------------------------------------
{
  // Perform LU factorization using 
  // appropriate AMD re-ordering mode:
  //---------------------------------------
  // 0: natural: C = A     (no reordering)
  // 1: Chol   : C = A+A'
  // 2: LU     : C = A'*A  (drop dense rows)
  // 3: QR     : C = A'*A
  // 4: Chol#2 : C = A     (A is symmetric)
  //---------------------------------------

  // clear existing system
  if (S) { delete S; S = NULL; }
  if (N) { delete N; N = NULL; }

  // check matrix input
  if (!A.ok())        {umERROR("CS_LU::lu", "empty matrix"); return 0;}
  if (!A.is_csc())    {umERROR("CS_LU::lu", "expected csc form"); return 0;}
  if (!A.is_square()) {umERROR("CS_LU::lu", "matrix must be square"); return 0;}

  try {
    // ordering and symbolic analysis
    S = CS_sqr(order, A, 0);
    if (!S) { umERROR("CS_LU::lu", "error building symbolic info"); return -1;}
  } catch(...) {
    umERROR("CS_LU::lu", "exception in symbolic phase"); return -1;
  }

  try {
    // numeric LU factorization
    N = CS_lu(A, S, tol);
    if (!N) { umERROR("CS_LU::lu", "error building numeric data"); return -2;}
  } catch(...) {
    umERROR("CS_LU::lu", "exception in numeric phase"); return -2;
  }

  return 1;
}


//---------------------------------------------------------
DVec& CS_LU::solve(const DVec& rhs) 
//---------------------------------------------------------
{
  // use factored form to solve for rhs, return x=A\rhs

  // check {symbolic, numeric} data is ready
  if (!S || !N) {umERROR("CS_LU::solve", "system not factorized");}

  // allocate arrays
  int n=rhs.size(); b=rhs; x.resize(n);
  if (!b.ok()||!x.ok()) {umERROR("CS_LU::solve", "out of memory");}
  if (N->L.n != n)      {umERROR("CS_LU::solve", "rhs not compatible");}

  CS_ipvec (N->pinv, b, x, n);  // x = b(p)
  CS_lsolve(N->L,    x);        // x = L\x
  CS_usolve(N->U,    x);        // x = U\x
  CS_ipvec (S->Q,    x, b, n);  // b(q) = x
  return b; 
}


//---------------------------------------------------------
DMat& CS_LU::solve(const DMat& RHS) 
//---------------------------------------------------------
{
  // use factored form to solve for MULTIPLE rhs's.
  // return X=A\RHS

  if (!RHS.ok())      {umERROR("CS_LU::solve", "empty RHS");}
  int n=RHS.num_rows(), Nrhs=RHS.num_cols(); 
  // check {symbolic, numeric} data is ready
  if (!S || !N)       {umERROR("CS_LU::solve", "system not factorized");}
  if (N->L.n != n)    {umERROR("CS_LU::solve", "RHS not compatible");}

  // allocate workspace and result
  x.resize(n); if (!x.ok()) {umERROR("CS_LU::solve", "out of memory");}
  DMat* B=new DMat(RHS, OBJ_temp,"A|B"); // copy rhs

  // FIXME: Check for zero matrix

  for (int j=1; j<=Nrhs; ++j) 
  {
    b.borrow(n, B->pCol(j));      // get B(j) as jth RHS
    CS_ipvec (N->pinv, b, x, n);  // x = b(p)
    CS_lsolve(N->L,    x);        // x = L\x
    CS_usolve(N->U,    x);        // x = U\x
    CS_ipvec (S->Q,    x, b, n);  // b(q) = x
  }
  return (*B); 
}


///////////////////////////////////////////////////////////
//
// Conjugate Gradients using cholinc preconditioner
//
///////////////////////////////////////////////////////////

// File: CS_Utils.cpp
void CS_symamd(CSd& cs_A, IVec& perm, IVec& pinv);

// CSd& taucs_dccs_factor_llt(CSd& userA, double droptol, int modified);
CSd& CS_Cholinc(CSd& userA, double droptol, int modified);


#define OUT_TO_MATLAB 0
#define APPLY_PERM    0

//---------------------------------------------------------
int CS_PCG::cholinc(CSd &sp, double droptol)
//---------------------------------------------------------
{
  m_droptol = droptol;
  // take ownership of input matrix
  this->A.own(sp);


#if (OUT_TO_MATLAB)
  {
    FILE* fp=fopen("z_A1.dat", "w");
    A.write_ML(fp);
    fclose(fp);
  }
#endif

  // check system
  if (!A.ok())          { umERROR("CS_PCG::cholinc", "empty coefficient matrix."); }
  if (!A.is_square())   { umERROR("CS_PCG::cholinc", "Matrix must be square."); }

  // new factorization: invalidate previous factor and solution
  m_oldsol = false;
  m_factor = false;

  // 1. find fill-reducing ordering
  // 2. permute system (tril(A)) 
  // 3. build incomplete Cholesky preconditioner

  stopwatch timer; timer.start();
  double t1=0.0,t2=0.0;

  if (1)
  {
    int n = A.n, modified_flag = 0;
    //double unit = (n-1.)+n;
    double unit = A.P[n];
    umLOG(1, "\n     -----------------------------------------------\n");
    umLOG(1,   " ==> CS_PCG fac: incomplete Cholesky factorization\n");

#if (APPLY_PERM)
    //-----------------------------------
    // 1. find fill-reducing ordering
    //-----------------------------------
    t1=timer.read(); CS_symamd(A,perm,pinv); t2=timer.read();
    if (!perm.ok()) { umERROR("CS_PCG::cholinc", "symamd failed\n"); return -1; } 
    else            { umLOG(1, "\tOrdering time = %10.3lf seconds\n", t2-t1); }
#endif


#if (APPLY_PERM)
    //-----------------------------------
    // 2. permute system (tril(A)) 
    //-----------------------------------
    // FIXME: transpose
    //  CS_symperm expects triu(A)
    //  CS_symamd  returns tril(A)
#if (1)
    umLOG(1, "\ttransposing A before symperm... ");
    t1 = timer.read(); A.transpose(1); umLOG(1, "(%.3lf secs)\n",timer.read()-t1);
#endif

    // uses inverse permutation
  //PAPT = taucs_dccs_permute_symmetrically(A, pinv.data());
    t1=timer.read(); CSd PAPT = CS_symperm(A, pinv, 1); t2=timer.read();
    if (!PAPT.ok()) { umERROR("CS_PCG::cholinc", "symperm failed\n"); return -1; } 
    else            { umLOG(1, "\tPermute time  = %10.3lf seconds\n",t2-t1); }

#if (OUT_TO_MATLAB)
  {
    FILE* fp=fopen("z_PAPT1.dat", "w");
    PAPT.write_ML(fp);
    fclose(fp);
  }
#endif  // output
#endif  // perm



#if (APPLY_PERM)
    //-----------------------------------
    // 3. calc cholinc preconditioner
    //-----------------------------------
    // FIXME: undo transpose
    //  CS_Cholinc expects tril(PAPT)
#if (1)
    umLOG(1, "\ttransposing PAP' before factorize... ");
    t1=timer.read(); PAPT.transpose(1); umLOG(1, "(%.3lf secs)\n",timer.read()-t1);
#endif


#if (OUT_TO_MATLAB)
  {
    FILE* fp=fopen("z_PAPT2.dat", "w");
    PAPT.write_ML(fp);
    fclose(fp);
  }
#endif  // output
#endif  // perm


    t1 = timer.read();
  //this->L = CS_Cholinc(PAPT,droptol,modified_flag);
    this->L = CS_Cholinc(this->A,droptol,modified_flag);
    t2 = timer.read();
    if (!this->L.ok()) { umERROR("CS_PCG::cholinc", "factor_llt failed\n"); return -1; } 
    double curr = L.P[n];
    umLOG(1, " ==> CS_PCG fac: nnz(A): %11.0lf\n"
             "                 nnz(L): %11.0lf  d.tol %0.1e\n"
             "                 fillin: %11.0lf  ratio %0.3lf\n"
             "                   time: %11.2lf  seconds\n",
              unit, curr, droptol, curr-unit, curr/unit, t2-t1);
    umLOG(1, "     -----------------------------------------------\n\n");
  }

#if (OUT_TO_MATLAB)
  {
    FILE* fp=fopen("z_L.dat", "w");
    this->L.write_ML(fp);
    fclose(fp);
  }
  umERROR("Nigel", "Compare with Matlab");
#endif

  m_factor = true;  // cholinc() factor is now ready
  return 0;
}


#define SHOW_ITER_CONVERG 1

//---------------------------------------------------------
DVec& CS_PCG::solve(const DVec& rhs, double tol, int maxit)
//---------------------------------------------------------
{
  // Use a preconditioned Conjugate Gradient method 
  // to return an iterative solution to: x = A\rhs.
  //
  // 1. permute rhs
  // 2. solve using pcg
  // 3. unpermute result

#if (APPLY_PERM)
  m_permute = true;
#else
  m_permute = false;
#endif


  // check system
  if (!m_factor || !L.ok())  { umERROR("CS_PCG::solve", "cholinc factor not ready."); }

  // store user args
  m_tol=tol;  m_maxit=maxit;

  // store permuted rhs in pb
  int n=rhs.size(); pb.resize(n);
  if (m_permute) {
    CS_ipvec(this->pinv, rhs, pb, n);   // pb = P*rhs
  } else {
    pb = rhs;                           // pb = rhs
  }

  if (!pb.ok())       { umERROR("CS_PCG::solve", "failed to permute rhs"); }
  if (this->L.n != n) { umERROR("CS_PCG::solve", "rhs not compatible"); }

  //---------------------------------------------
  // When used during time-dependent simulations,
  // set the initial solution vector to zero, but
  // reuse previous solution on subsequent calls.
  //---------------------------------------------
  
  // work with permuted px = P(x), 
  // return unpermuted   x = P(px),
  
  px.resize(n, false);  // false -> don't bother initialising
  if (!m_oldsol) {
    px.fill(0.0);       // initial guess is zero vector
     x.resize(n);       // allocate return vector
  } 
  else 
  {
    if (m_permute) {
      // reapply permutation and use old solution as inital guess
      CS_ipvec(this->pinv, x, px, n);   // px = P*x
    } else {
      px = x;                           // px = x
    }
  }

  if (!px.ok() || !x.ok()) { umERROR("CS_PCG::solve", "out of memory"); }

  // check parameters
  if (m_tol<=0.0) { m_tol = 1e-6;           umWARNING("pcg", "resetting tol to %g  (was %g).", m_tol,   tol); }
  if (m_maxit>n)  { m_maxit=std::min(n,20); umWARNING("pcg", "setting maxit to %d  (was %d).", m_maxit, maxit); }

  // Check for all zero right hand side vector => all zero solution
  double n2b = pb.norm2();          // Norm of rhs vector, b
  if (0.0 == n2b) {                 // if rhs vector is all zeros
    x.resize(n,true,0.0);           // then  solution is all zeros
    m_flag   = 0;                   // a valid solution has been obtained
    m_relres = 0;                   // the relative residual is actually 0/0
    m_iter   = 0;                   // no iterations need be performed
    m_resvec = 0;                   // resvec(1) = norm(b-A*x) = norm(0)
  //if (m_verbose) {itermsg("pcg", m_tol,m_maxit,0,m_flag,m_iter,NaN);}
    return x;
  }

  // local variables
  DVec xmin("xmin"), r("r"), z("z"), p("p"), q("q"), b_Ax("b-Ax");
  double tolb=0.0,normr=0.0,normrmin=0.0,rho=0.0,rho1=0.0,pq=0.0;
  double alpha=0.0,beta=0.0;  int i=0, imin=0;
  // IVec stagtest(n, "stagtest"), ind("ind");

  //-------------------------------------------------------
  // Set up for pcg method
  //-------------------------------------------------------
  m_flag = 1;
  imin = 0;                   // iteration at which xmin was computed
  xmin = px;                  // iterate which has minimal residual so far
  tolb = m_tol * n2b;         // relative tolerance
  r = pb - A*px;
  normr = r.norm2();          // norm of residual

  if (normr <= tolb) {
    m_flag   = 0;             // initial guess "x0" was good enough.
    m_relres = normr / n2b;   // since we have made no changes to x,
    m_iter   = 0;             // just return old x without permuting
    m_resvec = normr;
  //if (m_verbose) {itermsg("pcg", m_tol,m_maxit,0,m_flag,m_iter,relres);}
  //CS_pvec(this->pinv, px, x, n); // unpermute solution
    m_oldsol = true;
    return x;
  }

  m_resvec.resize(m_maxit+1);   // Preallocate vector for norm of residuals
  m_resvec(1) = normr;          // resvec(1) = norm(b-A*x0)
  normrmin    = normr;          // Norm of minimum residual
  rho         = 1.0;
  bool stag   = false;          // stagnation: flag failure to converge
  bool bOk    = true;          // stagnation: flag failure to converge

  //-------------------------------------------------------
  // loop for maxit iters, unless convergence or failure:
  //-------------------------------------------------------
  for (i=1; i<=m_maxit; ++i) 
  {
    // apply cholinc preconditioner
    z = solve_LLT(r);       // z = LLT\r
  //bOk = solve_LLT(r,z);   // z = LLT\r
    if (isInf(z)) 
  //if (!bOk) 
    {
      m_flag = 2; break;
    }

    rho1=rho;  rho=inner(r,z);

    if ((0.0==rho) || isinf(rho)) {
      m_flag = 4; break;
    }

    if (1 == i) {
      p = z;
    } else {
      beta = rho / rho1;
      if ((0.0 == beta) || isinf(beta)) {
        m_flag = 4; break;
      }
    //p = z + beta * p;
      p*=beta;  p+=z;
    }

    q = A*p;
    pq = inner(p,q);

    if ((pq <= 0) || isinf(pq)) {
      m_flag = 4; break;
    } else {
      alpha = rho / pq;
    }

    if (isinf(alpha)) {
      m_flag = 4; break;
    }

    // Check for stagnation of the method
    if (0.0 == alpha) { stag = true; }

#if (0)
    //#####################################################
    // TODO: Check for stagnation of the method
    //#####################################################
    if (!stag) {
      stagtest.fill(0);
      ind = find(x, '!', 0.0);
      stagtest(ind) = dd(p(ind), x(ind));
      stagtest(~ind & p ~= 0) = Inf;
      if (abs(alpha)*norm(stagtest,inf) < eps) {stag = true;}
    }
    //#####################################################
#endif

    // form new iterate
    px += alpha * p;
    b_Ax = pb - A*px;
    normr = b_Ax.norm2();
    m_resvec(i+1) = normr;

    // check for convergence
    if (normr <= tolb) { 
      m_flag = 0; m_iter = i;

#if 1
      umLOG(1, " ==> CS_PCG sol: %3d %15.12lf\n", i, normr);
#endif

      break; 
    }

    // check for stagnation
    if (stag) { 
      m_flag = 3;
      break; 
    }

    // update minimal norm quantities
    if (normr < normrmin) { 
      normrmin = normr; xmin = px; imin = i; 
    }

    r -= alpha * q;

#if (SHOW_ITER_CONVERG)
    umLOG(1, " ==> CS_PCG sol: %3d %15.12lf\n", i, normr);
#endif

  } // for i=1:m_maxit

  // returned solution is first with minimal residual
  if (0 == m_flag) {
    m_relres = normr / n2b;
  } else {
    px = xmin; 
    m_iter = imin; 
    m_relres = normrmin/n2b;
  }

  // truncate the zeros from resvec
  if ((m_flag <= 1) || (m_flag == 3)) {
    m_resvec.truncate(i+1);
  } else {
    m_resvec.truncate(i);
  }

  // optional report
  if (m_verbose) {
    // itermsg('pcg',m_tol,m_maxit,i,m_flag,m_iter,relres);
  }

  if (m_permute) {
    // unpermute the solution
    CS_pvec(this->pinv, px, x, n);
  } else {
    x = px;
  }

#if (SHOW_ITER_CONVERG)
  umLOG(1, "\n");
#endif

  m_oldsol = true;
  return x;
}


//---------------------------------------------------------
DVec& CS_PCG::solve_LLT(const DVec& rhs)
//---------------------------------------------------------
{
  // return x = [LL']\rhs
  //
  // Note: the system and the incomplete factor may or 
  // may not be permuted.  This routine is called by the 
  // pcg method to accelerate convergence, so we don't 
  // want to permute and unpermute the rhs vector.  The 
  // calling pcg driver manages permutation of the rhs.

//assert(this->pinv.ok());
  assert(rhs.ok());
  assert(this->L.ok());
  assert(this->L.num_cols() == rhs.size());

  this->prec_x = rhs;  assert(prec_x.ok());

//CS_ipvec  (this->pinv, b, x, n); // x = P*b
  CS_lsolve (this->L,    prec_x);  // x = L\x
  CS_ltsolve(this->L,    prec_x);  // x = L'\x
//CS_pvec   (this->pinv, x, b, n); // b = P'*x
  return prec_x; 
}



///////////////////////////////////////////////////////////
//
// GMRES using incomplete LU preconditioner
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
int CS_GMRES::luinc(CSd &A, int order, double droptol)
//---------------------------------------------------------
{
  umERROR("CS_GMRES::luinc", "Not yet implemented");

  return 0;
}


//---------------------------------------------------------
DVec& CS_GMRES::solve(const DVec& rhs)
//---------------------------------------------------------
{
  // Use a preconditioned GMRES method 
  // to return an iterative solution to: x = A\rhs.
  //
  // 1. permute rhs
  // 2. solve using gmres
  // 3. unpermute result

  // CS_ipvec(S->pinv, b, x, n);   // x = P*b
  // CS_pcg  (A, pCh, ...);        // x = pcg(A, LLT, ...)
  // CS_pvec (S->pinv, x, b, n);   // b = P'*x

  // allocate solution (reference for syntax)
  DVec* pX = new DVec("x", OBJ_temp); DVec& x = *pX;
  x.set_mode(OBJ_real);   // avoid premature delete

  umERROR("CS_GMRES::solve", "Not yet implemented");

  return x;
}


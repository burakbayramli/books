// CHOLMOD_solver.cpp
// 
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"


#ifdef NDG_USE_CHOLMOD

#include "CHOLMOD_solver.h"
#include "Stopwatch.h"

stopwatch chol_timer;


// halt if an error occurs
//---------------------------------------------------------
static void CHOLMOD_ehandler(int status, char *file, int line, char *message)
//---------------------------------------------------------
{
  umLOG(1, "cholmod error: file: %s line: %d status: %d: %s\n",
            file, line, status, message);
  if (status < 0) {
    umERROR("CHOLMOD_ehandler", "Unrecoverable error in CHOLMOD");
  }
}


//---------------------------------------------------------
CHOLMOD_solver::CHOLMOD_solver()
//---------------------------------------------------------
  : initialized(false), drop_tol(0.0),
    m_status(0), m_NNZ(0), m_M(0), m_N(0),
    A(NULL), L(NULL), x(NULL), b(NULL)
{
  init_common();
}


//---------------------------------------------------------
CHOLMOD_solver::CHOLMOD_solver
(
  const CSd &mat,     // the matrix to factor
  double droptol      // optional droptol
)
//---------------------------------------------------------
  : initialized(false), drop_tol(0.0),
    m_status(0), m_NNZ(0), m_M(0), m_N(0),
    A(NULL), L(NULL), x(NULL), b(NULL)
{
  init_common();
  chol(mat, 0, droptol);
}


//---------------------------------------------------------
CHOLMOD_solver::~CHOLMOD_solver()
//---------------------------------------------------------
{
  reset();
}


//---------------------------------------------------------
void CHOLMOD_solver::reset()
//---------------------------------------------------------
{
  initialized = false;
  m_NNZ = m_M = m_N = 0;

  cholmod_free_factor(&L, cm);      // free matrices
  cholmod_free_sparse(&A, cm);
  cholmod_free_dense (&x, cm);
#if (0)
  // Note: b points into the allocation for B, so do NOT free!
  //cholmod_free_dense (&b, cm);
#endif

  cholmod_finish(cm);               // clear workspace
  L=NULL; A=NULL; x=NULL; b=NULL;   // invalidate pointers
  cm=&Common; // but Common is a member struct
}


//---------------------------------------------------------
void CHOLMOD_solver::init_common()
//---------------------------------------------------------
{
  cm = &Common;
  cholmod_start(cm);
  cm->error_handler = CHOLMOD_ehandler;
  chol_timer.reset(); chol_timer.start();

  this->b = cholmod_allocate_dense(1,1,1,CHOLMOD_REAL, cm);
}


//---------------------------------------------------------
void CHOLMOD_solver::chol(const CSd& mat, int dummy, double droptol)
//---------------------------------------------------------
{

  if (initialized) {
    reset();          // clear previous system
    init_common();    // reset common defaults
  }

  // check that matrix arg is valid 
  if (!mat.ok()) { umERROR("CHOLMOD_solver(CSd&)", "matrix arg is empty"); return; }

  // load CSd into CHOLMOD structures
  load(mat);

  // analyze
  double t1 = chol_timer.read(), ta=0.0, tf=0.0;
  this->L = cholmod_analyze(this->A, this->cm);
  ta = chol_timer.read() - t1;
  umMSG(1, "Analyze: flop %g lnz %g time %g\n", cm->fl, cm->lnz, ta);

  // factorize
  t1 = chol_timer.read();
  cholmod_factorize(this->A, this->L, this->cm);
  tf = chol_timer.read() - t1;
  umMSG(1, "Factor : flop %g lnz %g time %g\n", cm->fl, cm->lnz, tf);

  initialized = true;
}


//---------------------------------------------------------
DVec& CHOLMOD_solver::solve(const DVec &rhs)
//---------------------------------------------------------
{
  int n=rhs.size(); this->B=rhs; this->X.resize(n);
  if (!B.ok()||!X.ok()) {umERROR("CHOLMOD_solver::solve", "out of memory"); return X;}

  // update member data in (struct cholmod_dense) b.
  // Note that b->x borrows raw allocation in DVec B

  b->x=B.data(); b->nrow=n; b->ncol=1; b->nzmax=n; b->d=n; b->z=NULL;
  x = cholmod_solve(CHOLMOD_A, this->L, this->b, this->cm);

  X.copy(n, (double*)(x->x));   // copy "cholmod_dense" to "DVec"
  cholmod_free_dense (&x, cm);  // release this allocation
  return X;
}


//---------------------------------------------------------
void CHOLMOD_solver::write_matlab(const char* sz) const
//---------------------------------------------------------
{
#if (0)
  //#######################################################

  IVec Acol(m_NNZ, "Acol");
  col_to_triplet(m_N, Ap.data(), Acol.data());

  ofstream os(sz);
  for (int i=0; i<m_NNZ; ++i) {
    os << umOFORM("%7d %7d %25.15e\n", Ai[i]+1, Acol[i]+1, Ax[i]);
  }
  os << endl;

  //#######################################################
#endif
}


//---------------------------------------------------------
bool CHOLMOD_solver::load(const CSd& mat)
//---------------------------------------------------------
{
  // check inputs
  if (!mat.ok() || !mat.is_csc()) {
    umERROR("CHOLMOD_solver::load(CSd&)", "expected csc format"); 
    return false;
  } 

  // clear existing matrix
  if (this->A) {
    cholmod_free_sparse(&A, cm);
    A = NULL;
  }

  this->cm->status = CHOLMOD_OK;

  // get inputs
  int nrow   = mat.num_cols();
  int ncol   = mat.num_cols();
  int nzmax  = mat.nnz();
  int packed = mat.is_csc();
  int sorted = 1;             // FIXME: is mat->sorted?
  int stype  = -1;            // store lower triangle only
  int xtype  = CHOLMOD_REAL;

  // allocate the cholmod_sparse structure
  A = cholmod_allocate_sparse (nrow, ncol, nzmax, sorted, packed, stype, xtype, this->cm);

  if (cm->status < CHOLMOD_OK) {
    umERROR("CHOLMOD_solver::load(CSd&)", "out of memory"); 
    return false;
  }

  // copy the matrix
  int *Ap=(int*)(A->p), *Ai=(int*)(A->i), p=0,j=0,nz=0;
  double* Ax = (double*)(A->x);

  for (j=0; j<=ncol; ++j) {Ap[j]=mat.P[j];} nz=Ap[ncol];
  for (p=0; p<nz; ++p)    {Ai[p]=mat.I[p];}
  for (p=0; p<nz; ++p)    {Ax[p]=mat.X[p];}  // case CHOLMOD_REAL:

  return true;
}

#endif // NDG_USE_CHOLMOD

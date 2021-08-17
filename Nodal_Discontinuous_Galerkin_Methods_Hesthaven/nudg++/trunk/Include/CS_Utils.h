// CS_Utils.h
// declare utility routines used by CS structures
// 2007/10/06
//---------------------------------------------------------
#ifndef NDG__CS_Utils_H__INCLUDED
#define NDG__CS_Utils_H__INCLUDED

#include "Vec_Type.h"

// declare class CS<T>
template <typename T> class CS;
class CSS;  // symbolic
class CSN;  // numeric

IVec&   CS_counts(const CS<double>& A, const IVec& parent, const IVec& post, int ata);
double  CS_cumsum(IVec& p, IVec& c, int n);
int     CS_happly(const CS<double>& V, int i, double beta, DVec& x);
double  CS_house(double *x, double *beta, int n);
int     CS_ipvec(const IVec& p, const DVec& b, DVec& x, int n);
int     CS_pvec (const IVec& p, const DVec& b, DVec& x, int n);
int     CS_leaf(int i, int j, const int *first, int *maxfirst, int *prevleaf, int *ancestor, int *jleaf);
void    CS_randperm(int n, int seed, IVec& p);
IVec&   CS_pinv(const IVec& p, int n);
IVec&   CS_post(const IVec& parent, int n);


//---------------------------------------------------------
// triangular solves
//---------------------------------------------------------
int   CS_lsolve (const CS<double>& L, DVec& x);
int   CS_ltsolve(const CS<double>& L, DVec& x);
int   CS_usolve (const CS<double>& U, DVec& x);
int   CS_utsolve(const CS<double>& U, DVec& x);
int   CS_spsolve(CS<double>& G, const CS<double>& B, int k, IVec& xi, DVec& x, const IVec& pinv, int lo);

//---------------------------------------------------------
// utilites...
//---------------------------------------------------------
int   CS_reach(CS<double>& G, const CS<double>& B, int k, IVec& xi, const IVec& pinv);
int   CS_rprune (int i, int j, double aij, void *other);
void  CS_matched(int n, const int *wj, const int *imatch, IVec& p, IVec& q, int *cc, int *rr, int set, int mark);
void  CS_unmatched (int m, const int *wi, IVec& p, int *rr, int set);

bool  CS_bfs(const CS<double>& A, int n, int *wi, int *wj, IVec& queue, const int *imatch, const int *jmatch, int mark);
int   CS_dfs(int j, CS<double>& G, int top, IVec& xi, int *pstack, const int *pinv);
int   CS_tdfs(int j, int k, int *head, const int *next, int *post, int *stack);
int   CS_vcount(const CS<double> &A, CSS *S);
void  CS_augment(int k, const CS<double>& A, int *jmatch, int *cheap, IVec& w, int *js, int *is, int *ps);


//---------------------------------------------------------
// matrix reorderings
//---------------------------------------------------------
CS<double>&  CS_symperm(const CS<double>& A, const IVec& pinv, int values);
IVec& CS_amd(int order, const CS<double>& A);

//---------------------------------------------------------
// qr routines
//---------------------------------------------------------
CSS*  CS_sqr(int order, const CS<double>& A, int qr);
CSN*  CS_qr(const CS<double>& A, const CSS *S);
bool  CS_qrsol(int order, const CS<double>& A, DVec& b);

//---------------------------------------------------------
// lu routines
//---------------------------------------------------------
bool  CS_lusol(int order, const CS<double>& A, DVec& b, double tol);
CSN*  CS_lu(const CS<double>& A, const CSS *S, double tol);


//---------------------------------------------------------
// Cholesky routines
//---------------------------------------------------------
bool  CS_cholsol(int order, CS<double>& A, DVec& b);
CSS*  CS_schol(int order, const CS<double>& A);
CSN*  CS_chol(CS<double>& A, const CSS *S, bool own_A=false);

//---------------------------------------------------------
// macros
//---------------------------------------------------------
#define CS_FLIP(i) (-(i)-2)
#define CS_UNFLIP(i) (((i) < 0) ? CS_FLIP(i) : (i))
#define CS_MARKED(w,j) (w[j]<0)
#define CS_MARK(w,j) { w[j]=CS_FLIP(w[j]); }



// typedef struct cs_dmperm_results {...} csd;
// cs_dmperm or cs_scc output
//---------------------------------------------------------
class cs_dp
//---------------------------------------------------------
{
public:
  cs_dp(int m, int n) 
  {
    P.resize(std::max(m,1));
    R.resize(m+6);
    Q.resize(std::max(n,1));
    S.resize(n+6);

    // init coarse row/col decompositions
    for (int i=0; i<5; ++i) {rr[i]=cc[i]=0;}

#ifndef NDEBUG
    assert(P.ok() && Q.ok());
    assert(R.ok() && S.ok()); 
#endif

  }

  ~cs_dp() {}

  int  get_mode() const    { return m_mode; }
  void set_mode(int mode)  { m_mode = mode; }
  bool ok() const { return true; }

  IVec P;       // size m, row permutation
  IVec Q;       // size n, column permutation
  IVec R;       // size nb+1, block k is rows r[k] to r[k+1]-1 in A(p,q)
  IVec S;       // size nb+1, block k is cols s[k] to s[k+1]-1 in A(p,q)
  int  nb;      // # of blocks in fine dmperm decomposition
  int  rr[5];   // coarse row decomposition
  int  cc[5];   // coarse column decomposition
  int  m_mode;  // {OBJ_real,OBJ_temp}
};


#endif // NDG__CS_Utils_H__INCLUDED

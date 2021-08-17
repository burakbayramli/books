// CHOLMOD_solver.h
// Test wrapper for CHOLMOD
// 2007/06/06
//---------------------------------------------------------
#ifndef NDG__CHOLMOD_solver_H__INCLUDED
#define NDG__CHOLMOD_solver_H__INCLUDED


#include "cholmod.h"
#include "CS_Type.h"

//---------------------------------------------------------
class CHOLMOD_solver
//---------------------------------------------------------
{
public:

  CHOLMOD_solver();
  CHOLMOD_solver(const CSd &mat, double droptol=0.0);
  virtual ~CHOLMOD_solver();

  void reset();
  void init_common();

  // interface compatibility
  int   num_rows() const { return m_M; }
  int   num_cols() const { return m_N; }
  void  set_droptol(double d)  { drop_tol = d; }
  void  write_matlab(const char* sz) const;

  // load/factor/solve
  bool  load(const CSd& mat);
  void  chol(const CSd &mat, int dummy=1, double droptol=0.0);
  DVec& solve(const DVec &b);

protected:
  bool    initialized;
  double  drop_tol;
  int     m_status, m_NNZ, m_M, m_N;

  cholmod_sparse  *A;
  cholmod_factor  *L;
  cholmod_dense   *x, *b; // solution, rhs
  cholmod_common  Common, *cm;

  DVec B, X; // rhs, solution
};

#endif  // INCLUDED

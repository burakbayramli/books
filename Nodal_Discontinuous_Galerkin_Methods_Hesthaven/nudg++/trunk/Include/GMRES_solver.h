// GMRES_solver.h
// 
// 2007/01/07
//---------------------------------------------------------
#ifndef NDG__GMRES_solver_H__INCLUDED
#define NDG__GMRES_solver_H__INCLUDED

// sparse matrix
#include "CS_Type.h"


//---------------------------------------------------------
class GMRES_solver
//---------------------------------------------------------
{
protected:
  CSd*    m_pA;         // pointer to system
  CS_LU*  m_precSol;    // TODO: iLU preconditioner
  
  double m_droptol;
  bool   precond_ok;

public:
  GMRES_solver() 
    : m_pA(NULL), m_precSol(NULL), m_droptol(1e-6), precond_ok(false) 
  {}

  GMRES_solver(CSd& A, double droptol=1e-6)
    : m_pA(&A), m_precSol(NULL), m_droptol(droptol), precond_ok(false) 
  { 
    init(A, droptol);
  }

  void reset() {
    m_pA = NULL; precond_ok = false;
    if (m_precSol) { delete m_precSol; m_precSol=NULL; }
  }

  void init(CSd& A, double droptol=1e-6) 
  {
    reset();
    try {
      m_pA = &A;                  // pointer to system
      m_precSol = new CS_LU;      // create preconditioner
    //m_precSol->ilu(droptol);    // prepare iLU preconditioner
      m_precSol->lu(*m_pA);       // prepare iLU preconditioner
      precond_ok = true;
    } catch(...) {
      precond_ok = false;
      umERROR("GMRES_solver::init()", "failed to create preconditioner");
    }
  }

  void solve (umDVec&   b,        // right hand side
              umDVec&   x,        // init/final solution
              umIVec&   iter,     // return {inner,outer} iterations
              int restart = 10,   // after "restart" iterations, restart algorithm 
              double  tol = 1e-6, // ] tolerance of the method
              int   maxit = 50);  // max number of outer iterations
};

#endif  // NDG__GMRES_solver_H__INCLUDED

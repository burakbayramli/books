// CurvedINSPressureSetUp2D.m
// function [PRperm, PRsystemC, PRsystemCT, rhsbcPR] = ...
//            CurvedINSPressureSetUp2D(dt, nu, BCfunction)
// 2007/06/10
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedINS2D.h"


//---------------------------------------------------------
void CurvedINS2D::CurvedINSPressureSetUp2D()
//---------------------------------------------------------
{
  //---------------------------
  double t1 = timer.read();
  //---------------------------

  // Note: assumes {m_cub,m_gauss} have been defined
  Cub2D&   cub = this->m_cub;
  Gauss2D& gauss = this->m_gauss;

  // function [PRperm, PRsystemC, PRsystemCT, rhsbcPR] = ...
  //   CurvedINSPressureSetUp2D(dt, nu, BCfunction)
  //
  // Purpose: build pressure system and boundary forcing term

  CSd *PRsystemBC = new CSd("PRbc");  // NBN: delete before chol()
  CSd *mm = new CSd("mmP");           // NBN: delete before chol()
  CSd PRsystem("PR"); IVec ids;       // NBN: passed to chol()

  // save original boundary types
  saveBCType = BCType;

  // Convert {Out} boundary conditions to Dirichlet
  ids = find(saveBCType, '=', (int)BC_Out);   BCType(ids) = BC_Dirichlet;
  
  // Convert {In,Wall,Cyl} boundary conditions to Neumann
  ids = find(saveBCType, '=', (int)BC_In );   BCType(ids) = BC_Neuman;
  ids = find(saveBCType, '=', (int)BC_Wall);  BCType(ids) = BC_Neuman;
  ids = find(saveBCType, '=', (int)BC_Cyl);   BCType(ids) = BC_Neuman;

  // Check: dumpIMat(BCType, "Pressure BCType");

  // Form inhomogeneous boundary term for rhs data
  (this->*ExactSolutionBC)(gauss.x, gauss.y, gauss.nx, gauss.ny, 
             gauss.mapI, gauss.mapO, gauss.mapW, gauss.mapC, 0.0, nu,
             bcUx, bcUy, bcPR, bcdUndt);

  // Build pressure boundary condition forcing vector
  CurvedPoissonIPDGbc2D(m_gauss, (*PRsystemBC));
  refrhsbcPR = (*PRsystemBC) * bcPR;
  delete PRsystemBC; PRsystemBC=NULL;

  // Build pressure system (all Neumann, excluding outflow)
  CurvedPoissonIPDG2D(m_gauss, m_cub, PRsystem, (*mm));
  delete mm; mm=NULL;


#if (0)
  // check against Matlab
  FILE* fp = fopen("nnP.dat", "w");
  PRsystem.write_ML(fp); fclose(fp);
#endif

  //-------------------------------------
  // factor Pressure Op
  //-------------------------------------
  PRsystemC->chol(PRsystem, 4);   // 4=CS_Chol option

  PRsystem.reset();               // force immediate deallocation
  BCType = saveBCType;            // Restore original boundary types

  //---------------------------
  time_setup += timer.read() - t1;
  //---------------------------
}

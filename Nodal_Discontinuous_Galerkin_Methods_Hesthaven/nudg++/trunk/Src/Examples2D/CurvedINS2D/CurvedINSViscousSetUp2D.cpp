// CurvedINSViscousSetUp2D.m
// function [VELperm, VELsystemC, VELsystemCT, rhsbcUx, rhsbcUy] = ...
//            CurvedINSViscousSetUp2D(dt, nu, g0, BCfunction)
// 2007/06/10
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedINS2D.h"


//---------------------------------------------------------
void CurvedINS2D::CurvedINSViscousSetUp2D()
//---------------------------------------------------------
{
  //---------------------------
  double t1 = timer.read();
  //---------------------------

  // Note: assumes {cub,gauss} have been defined
  Cub2D&   cub = this->m_cub;
  Gauss2D& gauss = this->m_gauss;

  // function [VELperm, VELsystemC, VELsystemCT, rhsbcUx, rhsbcUy] = ...
  //      CurvedINSViscousSetUp2D(dt, nu, g0, BCfunction)
  //
  // Purpose: build velocity system and boundary forcing terms

  CSd *VELsystemBC = new CSd("VELbc");  // NBN: delete before chol()
  CSd *mm = new CSd("mmV");             // NBN: delete before chol()
  CSd VELsystem("VEL"); IVec ids;       // NBN: passed to chol()

  // save original boundary types
  saveBCType = BCType;

  // Convert {In,Wall,Cyl} boundary conditions to Dirichlet
  ids = find(saveBCType, '=', (int)BC_In );   BCType(ids) = BC_Dirichlet;
  ids = find(saveBCType, '=', (int)BC_Wall);  BCType(ids) = BC_Dirichlet;
  ids = find(saveBCType, '=', (int)BC_Cyl);   BCType(ids) = BC_Dirichlet;

  // Convert {Out} boundary conditions to Neuman
  ids = find(saveBCType, '=', (int)BC_Out);   BCType(ids) = BC_Neuman;

  // Check: dumpIMat(BCType, "Viscous BCType");

  // Form inhomogeneous boundary term for rhs data (assumes time independent forcing)
  (this->*ExactSolutionBC)(gauss.x, gauss.y, gauss.nx, gauss.ny, 
              gauss.mapI, gauss.mapO, gauss.mapW, gauss.mapC, 0, nu,
              bcUx, bcUy, bcPR, bcdUndt);

  // Build viscous boundary condition forcing vector
  CurvedPoissonIPDGbc2D(gauss, (*VELsystemBC));
  refrhsbcUx = (*VELsystemBC) * bcUx;
  refrhsbcUy = (*VELsystemBC) * bcUy;
  delete VELsystemBC; VELsystemBC=NULL;

  // Build velocity system 
  CurvedPoissonIPDG2D(gauss, cub, VELsystem, (*mm));
  VELsystem += (*mm) * (g0/(dt*nu));
  delete mm; mm=NULL;

#if (0)
  // check against Matlab
  FILE* fp = fopen("nnV.dat", "w");
  VELsystem.write_ML(fp); fclose(fp);
#endif

  //-------------------------------------
  // factor Velocity Op
  //-------------------------------------
  VELsystemC->chol(VELsystem, 4);   // 4=CS_Chol option

  VELsystem.reset();                // force deallocation
  BCType = saveBCType;              // Restore original boundary types

  //---------------------------
  time_setup += timer.read() - t1;
  //---------------------------
}

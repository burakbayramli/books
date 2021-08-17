// CurvedDGJump2D.m
// function [jumpU] = CurvedDGJump2D(gU, gmapD, bcU)
// 2007/06/25
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"


//---------------------------------------------------------
DMat& NDG2D::CurvedDGJump2D
(
  const DMat& gU,     // [in]
  const IVec& gmapD,  // [in]
  const DVec& bcU     // [in]
)
//---------------------------------------------------------
{
  // function [jumpU] = CurvedDGJump2D(gU, gmapD, bcU)
  // purpose: compute discontinuous Galerkin jump applied
  //          to a field given at cubature and Gauss nodes

  DMat mmCHOL;  DVec gUM,gUP,fx;
  DMat *tmp = new DMat("jumpU", OBJ_temp);
  DMat &jumpU(*tmp);

  // shorthand references
  Cub2D& cub = this->m_cub; Gauss2D& gauss = this->m_gauss;

  // surface traces at Gauss nodes
  gUM = gU(gauss.mapM);
  gUP = gU(gauss.mapP);
  if (gmapD.size()>0) { gUP(gmapD) = bcU(gmapD); }

  // compute jump term and lift to triangle interiors
  fx = gUM - gUP;
  jumpU = -gauss.interpT*(gauss.W.dm(fx));

  // multiply straight sided triangles by inverse mass matrix
  jumpU(All,straight) = VVT * dd(jumpU(All,straight), J(All,straight));

  // multiply by custom inverse mass matrix for each curvilinear triangle
  int Ncurved = curved.size(), k=0;
  for (int m=1; m<=Ncurved; ++m) {
    k = curved(m);
    mmCHOL.borrow(Np,Np, cub.mmCHOL.pCol(k));
    mmCHOL.set_factmode(FACT_CHOL);  // indicate factored state
    jumpU(All,k) = chol_solve(mmCHOL, jumpU(All,k));
  }

  // these parameters may be OBJ_temp (allocated on the fly)
  if (OBJ_temp == gU.get_mode())  { delete (&gU); }
  if (OBJ_temp == bcU.get_mode()) { delete (&bcU); }

  return jumpU;
}

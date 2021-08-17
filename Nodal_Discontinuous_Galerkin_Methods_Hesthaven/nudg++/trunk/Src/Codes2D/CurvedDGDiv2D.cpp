// CurvedDGGrad2D.m
// function [divU] = CurvedDGDiv2D(cU, cV, gU, gV, gmapN, bcNdotU)
// 2007/06/25
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"


//---------------------------------------------------------
DMat& NDG2D::CurvedDGDiv2D
(
  const DMat& cU,       // [in]
  const DMat& cV,       // [in]
  const DMat& gU,       // [in]
  const DMat& gV,       // [in]
  const IVec& gmapN,    // [in]
  const DVec& bcNdotU   // [in]
)
//---------------------------------------------------------
{
  // function [divU] = CurvedDGDiv2D(cU, cV, gU, gV, gmapN, bcNdotU)
  // Purpose: compute the divergence of a vectorial function given
  //          at cubature and surface Gauss nodes

  DMat gFxM,gFxP,mmCHOL;  DVec gUM,gUP,gVM,gVP;
  DMat *tmp = new DMat("divU", OBJ_temp);
  DMat &divU(*tmp);

  // shorthand references
  Cub2D& cub = this->m_cub; Gauss2D& gauss = this->m_gauss;

  // Volume terms: U
  divU = cub.DrT * (cub.W.dm(cub.rx.dm(cU) + cub.ry.dm(cV))) + 
         cub.DsT * (cub.W.dm(cub.sx.dm(cU) + cub.sy.dm(cV)));

  // Surface traces at Gauss nodes
  gUM = gU(gauss.mapM); gUP = gU(gauss.mapP); 
  gVM = gV(gauss.mapM); gVP = gV(gauss.mapP);

  // Normal fluxes
  gFxM = gauss.nx.dm(gUM) + gauss.ny.dm(gVM);
  gFxP = gauss.nx.dm(gUP) + gauss.ny.dm(gVP);

  // Apply boundary conditions
  if (gmapN.size()>0) { gFxP(gmapN) = bcNdotU(gmapN); }
   
  // Add flux terms to divergence
  divU -= gauss.interpT * (gauss.W.dm(gFxM + gFxP))/2.0;

  // Multiply straight sided triangles by inverse mass matrix
  divU(All,straight) = VVT * dd(divU(All,straight), J(All,straight));

  // Multiply curvilinear faces by custom inverse mass matrix
  int Ncurved = curved.size(), k=0;
  for (int m=1; m<=Ncurved; ++m) {
    k = curved(m);
    mmCHOL.borrow(Np,Np, cub.mmCHOL.pCol(k));
    mmCHOL.set_factmode(FACT_CHOL);  // indicate factored state
    divU(All,k) = chol_solve(mmCHOL, divU(All,k));
  }

  // Correct sign
  divU *= -1.0;
  return divU;
}

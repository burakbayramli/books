// CurvedDGGrad2D.m
// function [dUdx, dUdy] = CurvedDGGrad2D(cU, gU, gmapD, bcU)
// 2007/06/25
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"


//---------------------------------------------------------
void NDG2D::CurvedDGGrad2D
(
  const DMat& cU,       // [in]
  const DMat& gU,       // [in]
  const IVec& gmapD,    // [in]
  const DVec& bcU,      // [in]
        DMat& dUdx,     // [out]
        DMat& dUdy      // [out]
)
//---------------------------------------------------------
{
  // function [dUdx, dUdy] = CurvedDGGrad2D(cU, gU, gmapD, bcU)
  // Purpose: compute DG derivative of field given at 
  //          cubature volume and Gauss surface nodes

  // shorthand references
  Cub2D& cub = this->m_cub; Gauss2D& gauss = this->m_gauss;

  DMat fx,fy,mmCHOL;  DVec gUM,gUP;

  // Volume terms: dUdx and dUdy
  dUdx = cub.DrT*(cub.W.dm(cub.rx.dm(cU))) + cub.DsT*(cub.W.dm(cub.sx.dm(cU)));
  dUdy = cub.DrT*(cub.W.dm(cub.ry.dm(cU))) + cub.DsT*(cub.W.dm(cub.sy.dm(cU)));

  // Surface traces at Gauss nodes
  gUM = gU(gauss.mapM);
  gUP = gU(gauss.mapP);

  // Apply boundary conditions
  if (gmapD.size()>0) { gUP(gmapD) = bcU(gmapD); }
   
  // Normal flux terms
  fx = 0.5*(gauss.nx.dm(gUM + gUP));
  fy = 0.5*(gauss.ny.dm(gUM + gUP));

  // Add lifted flux terms to volume terms
  dUdx -= gauss.interpT*(gauss.W.dm(fx));
  dUdy -= gauss.interpT*(gauss.W.dm(fy));

  // Multiply straight sided triangles by inverse mass matrix
  dUdx(All,straight) = VVT * dd(dUdx(All,straight), J(All,straight));
  dUdy(All,straight) = VVT * dd(dUdy(All,straight), J(All,straight));

  // Multiply curvilinear faces by custom inverse mass matrix
  int Ncurved = curved.size(), k=0;
  for (int m=1; m<=Ncurved; ++m) {
    k = curved(m);
    mmCHOL.borrow(Np,Np, cub.mmCHOL.pCol(k));
    mmCHOL.set_factmode(FACT_CHOL);  // indicate factored state
    dUdx(All,k) = chol_solve(mmCHOL, dUdx(All,k));
    dUdy(All,k) = chol_solve(mmCHOL, dUdy(All,k));
  }

  // Correct sign
  dUdx *= -1.0;
  dUdy *= -1.0;
}

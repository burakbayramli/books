// CurvedEuler2DRHS.m
// CurvedEuler2DRHS.m
// function [rhsQ] = CurvedEulerRHS2D(Qin, time, SolutionBC, fluxtype)
// 2007/06/22
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedEuler2D.h"


//---------------------------------------------------------
void CurvedEuler2D::RHS(DMat& Qin, double ti, fp_BC SolutionBC)
//---------------------------------------------------------
{
  // function [rhsQ] = CurvedEulerRHS2D(Qin, time, SolutionBC, fluxtype)
  // purpose: compute right hand side residual for the compressible Euler gas dynamics equations

  DMat qn, ddr, dds, Fn, Gn, mmCHOL, tm1;
  trhs = timer.read();  // time RHS work

  Cub2D&   cub   = this->m_cub;
  Gauss2D& gauss = this->m_gauss;

  int Nc=cub.Ncub, Ng=gauss.NGauss, n=0, i=0, m=0, k=0;
  int Nstraight=straight.size(), Ncurved=curved.size(); Index1D II;

  // 1.1 Interpolate solution to cubature nodes 
  for (n=1; n<=4; ++n) {
    qn.borrow(Np,K, Qin.pCol(n));
    cQ(All,n) = cub.V*qn;
  }

  // 1.2 Evaluate flux function at cubature nodes
  this->Fluxes(cQ, cF, cG);

  // 1.3 Compute volume terms (dphidx, F) + (dphidy, G)
  for (n=1; n<=4; ++n) {
    Fn.borrow(Nc,K, cF.pCol(n)); Gn.borrow(Nc,K, cG.pCol(n));
    ddr = cub.DrT * (cub.W.dm(cub.rx.dm(Fn) + cub.ry.dm(Gn)));
    dds = cub.DsT * (cub.W.dm(cub.sx.dm(Fn) + cub.sy.dm(Gn)));
    rhsQ(All,n) = ddr + dds;
  }

  // 2.1 SURFACE TERMS (using Gauss nodes on element faces)
  // See MapGaussFaceData()

  // 2.2 Interpolate solution to Gauss surface nodes
  for (n=1; n<=4; ++n) {
    qn.borrow(Np,K, Qin.pCol(n));
    gQ = gauss.interp*qn;
    gQM(All,n) = gQ(gauss.mapM);
    gQP(All,n) = gQ(gauss.mapP);
  }

  // 2.3 Apply boundary conditions to '+' traces
  if (SolutionBC) {
    (this->*BCSolution)(gauss.x, gauss.y, gauss.nx, gauss.ny, 
                        gauss.mapI, gauss.mapO, gauss.mapW, gauss.mapC,
                        ti, gQP);
  }

  // 2.4 Evaluate surface flux functions with stabilization
  switch (flux_type) {

  case FT_LaxF: this->LF2D  (gauss.nx, gauss.ny, gQM, gQP, gamma, flux); break;
  case FT_Roe:  this->Roe2D (gauss.nx, gauss.ny, gQM, gQP, gamma, flux); break;
  case FT_HLL:  this->HLL2D (gauss.nx, gauss.ny, gQM, gQP, gamma, flux); break;

  default: umERROR("CurvedEuler2D::RHS", 
                    "unknown flux_type: %d", flux_type); break;
  }

  // 2.5 Compute surface integral terms
  for (n=1; n<=4; ++n) {
    tm1 = gauss.interpT*(gauss.W.dm(flux(All,n)));
    rhsQ(All,n) -= tm1;
  }

  // 3.1 Multiply by inverse mass matrix
  for (n=1; n<=4; ++n)
  {
    // 3.1.a Multiply straight sided elements by inverse mass matrix
    for (m=1; m<=Nstraight; ++m) {
      k = straight(m);  II.reset((k-1)*Np+1, k*Np);
      rhsQ(II, n) = VVT * dd(rhsQ(II,n), J(II));
    }

    // 3.1.b Multiply curvilinear elements by custom inverse mass matrices
    for (m=1; m<=Ncurved; ++m) {
      k = curved(m);  II.reset((k-1)*Np+1, k*Np);
      mmCHOL.borrow(Np,Np, cub.mmCHOL.pCol(k));
      mmCHOL.set_factmode(FACT_CHOL);  // indicate factored state
      rhsQ(II, n) = chol_solve(mmCHOL, rhsQ(II, n));
    }
  }

  time_rhs += (timer.read() - trhs);
}

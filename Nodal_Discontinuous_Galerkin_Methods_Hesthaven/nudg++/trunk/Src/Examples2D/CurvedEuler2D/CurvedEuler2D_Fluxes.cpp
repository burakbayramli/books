// Euler2D_Fluxes.m
// function [F,G,rho,u,v,p] = EulerFluxes2D(Q, gamma)
// 2007/06/21
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedEuler2D.h"


//---------------------------------------------------------
void CurvedEuler2D::Fluxes(DMat& Qin, DMat& F, DMat& G)
//---------------------------------------------------------
{
  // function [F,G,rho,u,v,p] = EulerFluxes(Qin, gamma)
  // Purpose: evaluate primitive variables and Euler flux functions

  int Nr=Qin.num_rows();  DVec rho, rhou, rhov, Ener, u, v, p;

  // extract conserved variables
  rho.borrow (Nr,Qin.pCol(1)); rhou.borrow(Nr,Qin.pCol(2));
  rhov.borrow(Nr,Qin.pCol(3)); Ener.borrow(Nr,Qin.pCol(4));

  // compute primitive variables
  u=rhou.dd(rho); v=rhov.dd(rho); p=gm1*(Ener-0.5*(rhou.dm(u)+rhov.dm(v)));

  // compute flux functions
  F(All,1) = rhou;
  F(All,2) = rhou.dm(u) + p;
  F(All,3) = rhov.dm(u);
  F(All,4) = u.dm(Ener+p);

  G(All,1) = rhov;
  G(All,2) = rhou.dm(v);
  G(All,3) = rhov.dm(v) + p;
  G(All,4) = v.dm(Ener+p);
}


//---------------------------------------------------------
void CurvedEuler2D::Fluxes
(
  DMat& Qin,    // [in]
  double gamma, // [in]
  DMat& F,      // [out]
  DMat& G,      // [out]
  DVec& rho,    // [out]
  DVec& u,      // [out]
  DVec& v,      // [out]
  DVec& p       // [out]
)
//---------------------------------------------------------
{     
  // function [F,G,rho,u,v,p] = EulerFluxes(Q, gamma)
  // Purpose: evaluate primitive variables and Euler flux functions

  // extract conserved variables
  int Nr=Qin.num_rows();  DVec rhou, rhov, Ener;
  rho = Qin(All,1);             // deep copy rho for return
  rhou.borrow(Nr,Qin.pCol(2));  // but borrow {ru,rv,E}
  rhov.borrow(Nr,Qin.pCol(3));
  Ener.borrow(Nr,Qin.pCol(4));

  // compute primitive variables
  u=rhou.dd(rho); v=rhov.dd(rho); p=(gamma-1)*(Ener-0.5*(rhou.dm(u)+rhov.dm(v)));

  // compute flux functions
  F(All,1) = rhou;
  F(All,2) = rhou.dm(u) + p;
  F(All,3) = rhov.dm(u);
  F(All,4) = u.dm(Ener+p);

  G(All,1) = rhov;
  G(All,2) = rhou.dm(v);
  G(All,3) = rhov.dm(v) + p;
  G(All,4) = v.dm(Ener+p);
}

// EulerFluxes3D.m
// function [F,G,H,rho,u,v,w,p] = EulerFluxes3D(Q, gamma)
// 2007/07/11
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "Euler3D.h"


//---------------------------------------------------------
void Euler3D::Fluxes(DMat& Qin, DMat& F, DMat& G, DMat& H)
//---------------------------------------------------------
{
  // function [F,G,H,rho,u,v,w,p] = EulerFluxes3D(Q, gamma)
  // Purpose: evaluate primitive variables and Euler flux functions

  DVec q1,q2,q3,q4,q5, u,v,w,p, rho,rhou,rhov,rhow,Ener;
  int Nr = Qin.num_rows(); 

  // extract conserved variables
  rho.borrow (Nr,Qin.pCol(1)); rhou.borrow(Nr,Qin.pCol(2));
  rhov.borrow(Nr,Qin.pCol(3)); rhow.borrow(Nr,Qin.pCol(4)); 
  Ener.borrow(Nr,Qin.pCol(5));

  // compute primitive variables
  u = rhou.dd(rho); v = rhov.dd(rho); w = rhow.dd(rho);
  p = gm1*(Ener - 0.5*(rhou.dm(u) + rhov.dm(v) + rhow.dm(w)));

  // compute flux functions
  F(All,1) = rhou;
  F(All,2) = rhou.dm(u) + p;
  F(All,3) = rhov.dm(u);
  F(All,4) = rhow.dm(u);
  F(All,5) = u.dm(Ener+p);

  G(All,1) = rhov;
  G(All,2) = rhou.dm(v);
  G(All,3) = rhov.dm(v) + p;
  G(All,4) = rhow.dm(v);
  G(All,5) = v.dm(Ener+p);

  H(All,1) = rhow;
  H(All,2) = rhou.dm(w);
  H(All,3) = rhov.dm(w);
  H(All,4) = rhow.dm(w) + p;
  H(All,5) = w.dm(Ener+p);
}


//---------------------------------------------------------
void Euler3D::Fluxes
(
  DMat& Qin,    // [in]
  DMat& F,      // [out]
  DMat& G,      // [out]
  DMat& H,      // [out]
  DVec& rho,    // [out]
  DVec& u,      // [out]
  DVec& v,      // [out]
  DVec& w,      // [out]
  DVec& p       // [out]
)
//---------------------------------------------------------
{     
  // function [F,G,H,rho,u,v,w,p] = EulerFluxes(Q, gamma)
  // Purpose: evaluate primitive variables and Euler flux functions

  // extract conserved variables
  int Nr=Qin.num_rows();  DVec rhou,rhov,rhow, Ener;
  rho = Qin(All,1);             // deep copy rho for return
  rhou.borrow(Nr,Qin.pCol(2));  // but borrow {ru,rv,rw,E}
  rhov.borrow(Nr,Qin.pCol(3));
  rhow.borrow(Nr,Qin.pCol(4));
  Ener.borrow(Nr,Qin.pCol(5));

  // compute primitive variables
  u = rhou.dd(rho); v = rhov.dd(rho); w = rhow.dd(rho);
  p = gm1*(Ener - 0.5*(rhou.dm(u) + rhov.dm(v) + rhow.dm(w)));

  // compute flux functions
  F(All,1) = rhou;
  F(All,2) = rhou.dm(u) + p;
  F(All,3) = rhov.dm(u);
  F(All,4) = rhow.dm(u);
  F(All,5) = u.dm(Ener+p);

  G(All,1) = rhov;
  G(All,2) = rhou.dm(v);
  G(All,3) = rhov.dm(v) + p;
  G(All,4) = rhow.dm(v);
  G(All,5) = v.dm(Ener+p);

  H(All,1) = rhow;
  H(All,2) = rhou.dm(w);
  H(All,3) = rhov.dm(w);
  H(All,4) = rhow.dm(w) + p;
  H(All,5) = w.dm(Ener+p);
}

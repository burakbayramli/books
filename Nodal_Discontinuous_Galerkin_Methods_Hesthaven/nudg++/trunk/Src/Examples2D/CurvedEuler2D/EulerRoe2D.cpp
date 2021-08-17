// EulerRoe2D.m
// function flux = EulerRoe2D(lnx, lny, QM, QP, gamma)
// 2007/06/21
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedEuler2D.h"


//---------------------------------------------------------
void CurvedEuler2D::Roe2D
(
  const DMat&   lnx, 
  const DMat&   lny, 
        DMat&   QM, 
        DMat&   QP, 
        double  gamma,
        DMat&   flux
)
//---------------------------------------------------------
{
  //---------------------------
  double tf1 = timer.read();
  //---------------------------

  // function flux = EulerRoe2D(lnx, lny, QM, QP, gamma)
  // Purpose: compute surface fluxes for Euler's equations using an
  //          approximate Riemann solver based on Roe averages

  static DMat fxQM,fyQM, fxQP,fyQP;

  DVec rhouM,rhovM,EnerM,  rhoM,uM,vM,pM;
  DVec rhouP,rhovP,EnerP,  rhoP,uP,vP,pP;
  DVec HM,HP,H, rho,u,v,h, c2,c, rhoMs,rhoPs, rhoMsPs;
  DVec dW1,dW2,dW3,dW4, fx2,fx3;

  int Ngf=QM.num_rows();
  fxQM.resize(Ngf,4, false); fyQM.resize(Ngf,4, false); 
  fxQP.resize(Ngf,4, false); fyQP.resize(Ngf,4, false);

  // Deep-copy {rhouM/P,rhovM/P}, but just borrow {EnerM/P}
  rhouM = QM(All,2); rhovM=QM(All,3); EnerM.borrow(Ngf,QM.pCol(4));
  rhouP = QP(All,2); rhovP=QP(All,3); EnerP.borrow(Ngf,QP.pCol(4));

  // Rotate "-" trace momentum to face normal-tangent coordinates
  QM(All,2) =  lnx.dm(rhouM) + lny.dm(rhovM);
  QM(All,3) = -lny.dm(rhouM) + lnx.dm(rhovM);

  // Rotate "+" trace momentum to face normal-tangent coordinates
  QP(All,2) =  lnx.dm(rhouP) + lny.dm(rhovP);
  QP(All,3) = -lny.dm(rhouP) + lnx.dm(rhovP);

  // Compute fluxes and primitive variables in rotated coordinates  
  this->Fluxes(QM, gamma,  fxQM,fyQM, rhoM,uM,vM,pM);
  this->Fluxes(QP, gamma,  fxQP,fyQP, rhoP,uP,vP,pP);

  // Compute enthalpy
  HM = (EnerM+pM).dd(rhoM);  HP = (EnerP+pP).dd(rhoP);

  // Compute Roe average variables
  rhoMs = sqrt(rhoM); rhoPs = sqrt(rhoP);
  rhoMsPs = rhoMs + rhoPs;

  rho = rhoMs.dm(rhoPs);
  u   = (rhoMs.dm(uM) + rhoPs.dm(uP)).dd(rhoMsPs);
  v   = (rhoMs.dm(vM) + rhoPs.dm(vP)).dd(rhoMsPs);
  H   = (rhoMs.dm(HM) + rhoPs.dm(HP)).dd(rhoMsPs);

  c2 = gm1 * (H - 0.5*(sqr(u)+sqr(v)));  c = sqrt(c2);

  // Riemann fluxes
  dW1 = -0.5*(rho.dm(uP-uM)).dd(c) + 0.5*(pP-pM).dd(c2);
  dW2 = (rhoP-rhoM) - (pP-pM).dd(c2);
  dW3 = rho.dm(vP-vM);
  dW4 = 0.5*(rho.dm(uP-uM)).dd(c) + 0.5*(pP-pM).dd(c2);

  dW1 = abs(u-c).dm(dW1);
  dW2 = abs(u  ).dm(dW2);
  dW3 = abs(u  ).dm(dW3);
  dW4 = abs(u+c).dm(dW4);

  // Form Roe fluxes
  DMat fx = (fxQP+fxQM)/2.0;

//fx(All,1) -= (dW1.*1        + dW2.*1             + dW3.*0 + dW4.*1       )/2.0;
//fx(All,2) -= (dW1.*(u-c)    + dW2.*u             + dW3.*0 + dW4.*(u+c)   )/2.0;
//fx(All,3) -= (dW1.*v        + dW2.*v             + dW3.*1 + dW4.*v       )/2.0;
//fx(All,4) -= (dW1.*(H-u.*c) + dW2.*(u.^2+v.^2)/2 + dW3.*v + dW4.*(H+u.*c))/2.0;

  fx(All,1) -= (dW1               + dW2                                   + dW4              )/2.0;
  fx(All,2) -= (dW1.dm(u-c)       + dW2.dm(u)                             + dW4.dm(u+c)      )/2.0;
  fx(All,3) -= (dW1.dm(v)         + dW2.dm(v)                 + dW3       + dW4.dm(v)        )/2.0;
  fx(All,4) -= (dW1.dm(H-u.dm(c)) + dW2.dm(sqr(u)+sqr(v))/2.0 + dW3.dm(v) + dW4.dm(H+u.dm(c)))/2.0;

  // rotate back to Cartesian
  flux = fx;    fx2.borrow(Ngf, fx.pCol(2)); fx3.borrow(Ngf, fx.pCol(3));
  flux(All,2) = lnx.dm(fx2) - lny.dm(fx3);
  flux(All,3) = lny.dm(fx2) + lnx.dm(fx3);

  //---------------------------
  time_flux += timer.read() - tf1;
  //---------------------------
}

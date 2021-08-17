// EulerHLL2D.m
// function flux = EulerHLL2D(lnx, lny, QM, QP, gamma)
// 2007/06/21
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedEuler2D.h"


//---------------------------------------------------------
void CurvedEuler2D::HLL2D
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
  // function flux = EulerHLL2D(lnx, lny, QM, QP, gamma)
  // Purpose: compute surface fluxes for Euler's equations using 
  //          an approximate Riemann solver based on Roe averages

  static DMat fxQM,fyQM, fxQP,fyQP, fx;

  DVec rhouM,rhovM,EnerM,  rhoM,uM,vM,pM;
  DVec rhouP,rhovP,EnerP,  rhoP,uP,vP,pP;
  DVec HM,HP,H, rho,u,v, c2,c,cM,cP,rhoMs,rhoPs, rhoMsPs;
  DVec SL,SR, t1,t2,t3, fx2,fx3, vzero;

  int Ngf=QM.num_rows();     vzero.zeros(Ngf);
  fxQM.resize(Ngf,4, false); fyQM.resize(Ngf,4, false); 
  fxQP.resize(Ngf,4, false); fyQP.resize(Ngf,4, false); 
  fx.resize  (Ngf,4, false); 
  

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

  HM = (EnerM+pM).dd(rhoM); cM = sqrt(gamma*pM.dd(rhoM));
  HP = (EnerP+pP).dd(rhoP); cP = sqrt(gamma*pP.dd(rhoP));

  // Compute Roe average variables
  rhoMs = sqrt(rhoM); rhoPs = sqrt(rhoP);
  rhoMsPs = rhoMs + rhoPs;

  rho = rhoMs.dm(rhoPs);
  u   = (rhoMs.dm(uM) + rhoPs.dm(uP)).dd(rhoMsPs);
  v   = (rhoMs.dm(vM) + rhoPs.dm(vP)).dd(rhoMsPs);
  H   = (rhoMs.dm(HM) + rhoPs.dm(HP)).dd(rhoMsPs);

  c2  = gm1 * (H - 0.5*(sqr(u)+sqr(v)));  c = sqrt(c2);

  // Compute estimate of waves speeds
  SL = min(uM-cM, u-c);
  SR = max(uP+cP, u+c);

  // Compute HLL flux
  t1 = (min(SR,vzero) - min(vzero,SL)).dd(SR-SL);
  t2 = 1.0 - t1;
  t3 = (SR.dm(abs(SL))-SL.dm(abs(SR))).dd(2.0*(SR-SL));

  for (int n=1; n<=4; ++n) {
    fx(All,n) = t1.dm(fxQP(All,n)) + t2.dm(fxQM(All,n)) - t3.dm(QP(All,n)-QM(All,n));
  }

  // rotate flux back into Cartesian coordinates
  flux = fx;    fx2.borrow(Ngf, fx.pCol(2)); fx3.borrow(Ngf, fx.pCol(3));
  flux(All,2) = lnx.dm(fx2) - lny.dm(fx3);
  flux(All,3) = lny.dm(fx2) + lnx.dm(fx3);
}

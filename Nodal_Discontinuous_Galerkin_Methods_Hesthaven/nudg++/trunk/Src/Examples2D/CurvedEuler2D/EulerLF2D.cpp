// EulerLF2D.m
// function flux = EulerLF2D(lnx, lny, QM, QP, gamma)
// 2007/06/21
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedEuler2D.h"


//---------------------------------------------------------
void CurvedEuler2D::LF2D
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
  // Function flux = EulerLF2D(nx, ny, QM, QP, gamma)
  // Purpose: compute Local Lax-Friedrichs/Rusonov fluxes for Euler equations

  static DMat fxM,fyM, fxP,fyP;  int Ngf=QM.num_rows();
  fxM.resize(Ngf,4, false); fyM.resize(Ngf,4, false); 
  fxP.resize(Ngf,4, false); fyP.resize(Ngf,4, false);

  DMat maxvel; DVec rhoM,uM,vM,pM, rhoP,uP,vP,pP;

  // Evaluate primitive variables & flux functions at '-' and '+' traces
  this->Fluxes(QM, gamma,  fxM,fyM, rhoM,uM,vM,pM);
  this->Fluxes(QP, gamma,  fxP,fyP, rhoP,uP,vP,pP);

  // Compute wave speed for  Lax-Friedrichs/Rusonov numerical fluxes
  maxvel = max( sqrt(sqr(uM)+sqr(vM)) + sqrt(abs(gamma*pM.dd(rhoM))), 
                sqrt(sqr(uP)+sqr(vP)) + sqrt(abs(gamma*pP.dd(rhoP))));
        
  NGauss = m_gauss.NGauss;
  maxvel.reshape(NGauss, Nfaces*K);
  maxvel = outer( ones(NGauss), maxvel.max_col_vals());
  maxvel.reshape(NGauss*Nfaces, K);

  // Compute + lift fluxes to volume residual
  for (int n=1; n<=4; ++n) {
    flux(All,n) = 0.5*(lnx.dm(fxP(All,n) + fxM(All,n)) + 
                       lny.dm(fyP(All,n) + fyM(All,n)) +
                       maxvel.dm(QM(All,n) - QP(All,n)));
  }

#if (0)
  dumpDMat(flux, "flux");
#endif
}

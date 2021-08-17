// IsentropicVortexIC3D.m
// function Q = IsentropicVortexIC3D(x, y, z, ti)
// 2007/07/11
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "Euler3D.h"


//---------------------------------------------------------
void Euler3D::IsentropicVortexIC3D
(
  const DVec&   xi,   // [in]
  const DVec&   yi,   // [in]
  const DVec&   zi,   // [in]
        double  ti,   // [in]
        DMat&   Qo    // [out]
)
//---------------------------------------------------------
{
  // function Q = IsentropicVortexIC3D(x, y, z, ti)
  //
  // Purpose: compute flow configuration given by
  //          http://www.cfd-online.com/Wiki/2-D_vortex_in_isentropic_flow
  //     Y.C. Zhou, G.W. Wei / Journal of Computational Physics 189 (2003) 159 (TW: p96.pdf)

  static DVec u,v,w;  DVec xmut, ymvt, rsqr, ex1r, tv1, rho1, p1;
  int Nr = Qo.num_rows(); u.resize(Nr); v.resize(Nr); w.resize(Nr);

  // reset static arrays to constant values
  u=1.0;  v=0.0;  w=0.0;

  // base flow parameters
  double xo=0.0, yo=0.0, beta=5.0;
  double fac = 16.0*gamma*pi*pi;

  xmut = xi - u*ti;   ymvt = yi - v*ti;
  rsqr = sqr(xmut-xo)+sqr(ymvt-yo);
  ex1r = exp(1.0-rsqr);

  // perturbed density
  u -= beta * ex1r.dm(ymvt-yo)/(2.0*pi);
  v += beta * ex1r.dm(xmut-xo)/(2.0*pi);
  w  = 0.0;

  tv1  = (1.0-(gm1*SQ(beta)*exp(2.0*(1.0-rsqr))/fac));
  rho1 = pow(tv1, 1.0/gm1);
  p1   = pow(rho1, gamma);

  Qo(All,1) = rho1;
  Qo(All,2) = rho1.dm(u);
  Qo(All,3) = rho1.dm(v);
  Qo(All,4) = rho1.dm(w);
  Qo(All,5) = p1/gm1 + 0.5*rho1.dm(sqr(u)+sqr(v)+sqr(w));
}

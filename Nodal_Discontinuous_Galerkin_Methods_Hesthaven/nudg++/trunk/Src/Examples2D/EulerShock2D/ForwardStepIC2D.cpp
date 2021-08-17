// ForwardStepIC2D.m
// function Q = ForwardStepIC2D(x2d, y2d, time)
// 2007/06/30
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "EulerShock2D.h"


//---------------------------------------------------------
void EulerShock2D::ForwardStepIC2D
(
  const DVec&   xi,   // [in]
  const DVec&   yi,   // [in]
        double  ti,   // [in]
        DMat&   Qo    // [out]
)
//---------------------------------------------------------
{
  // function Q = ForwardStepIC2D(x2d, y2d, time)
  //
  // Purpose: compute plane flow configuration 

  double rho,u,v,p, rhou,rhov,Ener;

  rho = gamma; 
  u   = 3.0;
  v   = 0.0;
  p   = 1.0;

  // M = |u|/c,  c = sqrt(gamma*p/rho)
  rhou = rho * u;
  rhov = rho * v;
  Ener = p/gm1 + (SQ(rhou)+SQ(rhov))/(2.0*rho);

  assert(Qo.num_rows() == Np*K);
  Qo(All,1) = rho;
  Qo(All,2) = rhou;
  Qo(All,3) = rhov;
  Qo(All,4) = Ener; 
}

// ChannelIC2D.cpp
// function [rho,rhou,rhov,Ener] = ChannelIC2D(xi, yi, ti);
// 2007/06/21
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedEuler2D.h"


//---------------------------------------------------------
void CurvedEuler2D::ChannelIC2D
(
  const DVec&   xi,   // [in]
  const DVec&   yi,   // [in]
        double  ti,   // [in]
        DMat&   Qo    // [out]
)
//---------------------------------------------------------
{
  // function [Q] = ChannelIC2D(x, y, time)
  // Purpose: Impose uniform plane flow 

  // Note: relies on gamma=1.5

  double mu = 1e-2, pbar=10.0, gamma15 = 1.5;

  Qo(All,1) = 1.0;
  Qo(All,2) = sqr(yi);
  Qo(All,3) = 0.0;
  Qo(All,4) = (2.0*mu*xi + pbar)/(gamma15-1.0) + 0.5*pow(yi,4.0);
}

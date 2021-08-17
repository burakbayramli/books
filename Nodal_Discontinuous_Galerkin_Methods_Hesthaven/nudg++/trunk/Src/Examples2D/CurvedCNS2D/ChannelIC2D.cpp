// ChannelIC2D.m
// function [Q] = ChannelIC2D(x, y, time);
// 2007/06/26
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedCNS2D.h"


//---------------------------------------------------------
void CurvedCNS2D::ChannelIC2D
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
  //
  // Quadratic shear flow, relies on gamma=1.5

  //-------------------------------------
  // adjust parameters
  //-------------------------------------
  this->mu    = 1e-2;
  this->pbar  = 10.0;
  this->gamma = 1.5;
  this->gm1   = 0.5;  // gamma-1

  //-------------------------------------
  // use wrappers to update Qo in-place
  //-------------------------------------
  DVec rho,rhou,rhov,Ener; int Nr=Np*K;
  rho.borrow (Nr,Qo.pCol(1)); rhou.borrow(Nr,Qo.pCol(2));
  rhov.borrow(Nr,Qo.pCol(3)); Ener.borrow(Nr,Qo.pCol(4));

  rho  = 1.0;
  rhou = sqr(y);
  rhov = 0.0;
  Ener = (2*mu*x + pbar)/gm1 + 0.5*pow(y,4.0);
}

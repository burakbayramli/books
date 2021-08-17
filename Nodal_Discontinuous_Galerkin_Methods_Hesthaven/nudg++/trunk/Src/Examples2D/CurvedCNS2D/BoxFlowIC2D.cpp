// BoxFlowIC2D.m
// function [rho, rhou, rhov, Ener] = BoxFlowIC2D(x, y, time)
// 2007/06/21
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedCNS2D.h"


//---------------------------------------------------------
void CurvedCNS2D::BoxFlowIC2D
(
  const DVec&   xi,   // [in]
  const DVec&   yi,   // [in]
        double  ti,   // [in]
        DMat&   Qo    // [out]
)
//---------------------------------------------------------
{
  // function Q = BoxFlowIC2D(x, y, time)
  //
  // Purpose: compute plane flow configuration 

  //-------------------------------------
  // adjust parameters
  //-------------------------------------
  this->gamma = 1.4;
  this->gm1   = 0.4;  // (gamma-1)

  //-------------------------------------
  // use wrappers to update Qo in-place
  //-------------------------------------
  DVec rho,rhou,rhov,Ener; int Nr=Np*K;
  rho.borrow (Nr, Qo.pCol(1));
  rhou.borrow(Nr, Qo.pCol(2));
  rhov.borrow(Nr, Qo.pCol(3));
  Ener.borrow(Nr, Qo.pCol(4));

  if (1) 
  {
    this->pref = 12.0;

    rho  =  1.0;
    rhou = -sin(2.0*pi*y);
    rhov =  sin(4.0*pi*x);
    Ener = pref/gm1 + 0.5*(sqr(rhou) + sqr(rhov)).dd(rho);
  } 
  else 
  {
    this->pref = 12.0;

    rho  = 1.0;
    rhou = 0.0;
    rhov = 0.0;
    Ener = pref/gm1 + 0.5 * rho.dm(exp(-4.0*(sqr(cos(pi*x))+sqr(cos(pi*y)))));
  }
}

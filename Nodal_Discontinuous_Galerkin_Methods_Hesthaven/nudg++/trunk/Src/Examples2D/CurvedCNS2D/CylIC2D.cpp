// CylIC2D.m
// function [Q] = CylIC2D(xi, yi, time);
// 2007/06/26
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedCNS2D.h"


//---------------------------------------------------------
void CurvedCNS2D::CylIC2D
(
  const DVec&   xi,   // [in]
  const DVec&   yi,   // [in]
        double  ti,   // [in]
        DMat&   Qo    // [out]
)
//---------------------------------------------------------
{
  // function  Q = CylIC2D(xi, yi, time);
  // Purpose: Impose uniform plane flow 

  // Example is Mach ** 0.4 ** flow in wind tunnel (gamma=1.4)

  //-------------------------------------
  // adjust parameters
  //-------------------------------------
  this->gamma = 1.4;
  this->gm1   = 0.4;  // (gamma-1)

  //-------------------------------------
  // Inflow conditions -- uniform inflow
  //-------------------------------------
  double rhoin=1.4, uin=0.4, vin=0.0, pin=1.0;
  double Ein = pin/gm1 + 0.5*rhoin*(SQ(uin)+SQ(vin));

  //-------------------------------------
  // use wrappers to update Qo in-place
  //-------------------------------------
  DVec rho,rhou,rhov,Ener; int Nr=Np*K;
  rho.borrow (Nr,Qo.pCol(1)); rhou.borrow(Nr,Qo.pCol(2));
  rhov.borrow(Nr,Qo.pCol(3)); Ener.borrow(Nr,Qo.pCol(4));

#if (0)
  rho  = rhoin;
  rhou = rhoin * SQ(1.0/0.41)*6.0*(yi+0.2).dm(0.41-(yi+0.2));
  rhov = 0.0;
  Ener = Ein + 0.5*(sqr(rhou)+sqr(rhov)).dd(rho);
#else
  rho  = rhoin;
  rhou = 0.0;
  rhov = 0.0;
  Ener = Ein + 0.5*(sqr(rhou)+sqr(rhov)).dd(rho);
#endif
}

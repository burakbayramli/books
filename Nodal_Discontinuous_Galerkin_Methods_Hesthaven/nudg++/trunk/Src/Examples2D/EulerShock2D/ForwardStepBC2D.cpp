// ForwardStepBC2D.m
// function Q = ForwardStepBC2D(xin, yin, nxin, nyin, mapI, mapO, mapW, mapC, Q, time);
// 2007/07/02
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "EulerShock2D.h"


//---------------------------------------------------------
void EulerShock2D::ForwardStepBC2D
(
  const DVec& xi, 
  const DVec& yi, 
  const DVec& nxi, 
  const DVec& nyi, 
  const IVec& tmapI, 
  const IVec& tmapO, 
  const IVec& tmapW, 
  const IVec& tmapC, 
        double ti, 
        DMat& Qio
)
//---------------------------------------------------------
{
  // function [Q] = ForwardStepBC2D(xin, yin, nxin, nyin, mapI, mapO, mapW, mapC, Q, time);
  // Purpose: Impose channel boundary conditions on 2D Euler equations on weak form

  // Example is Mach ** 3.0 ** flow in wind tunnel
  // gamma = 1.4;

  int Nr = Qio.num_rows(); DVec rho,rhou,rhov,Ener;
  DVec& nxin = const_cast<DVec&>(nxi);
  DVec& nyin = const_cast<DVec&>(nyi);

  // extract conserved variables
#if (0)
  rho=Qio(All,1); rhou=Qio(All,2); rhov=Qio(All,3); Ener=Qio(All,4);
#else
  // wrap current state data (columns of Qio)
  rho.borrow (Nr, Qio.pCol(1));
  rhou.borrow(Nr, Qio.pCol(2));
  rhov.borrow(Nr, Qio.pCol(3));
  Ener.borrow(Nr, Qio.pCol(4));
#endif

  //-------------------------------------------------------
  // Inflow conditions -- uniform inflow
  //-------------------------------------------------------
  double rhoin=gamma, uin=3.0, vin=0.0, pin=1.0;
  double Ein = pin/gm1 + 0.5*rhoin*(SQ(uin)+SQ(vin));
  rho(tmapI)=rhoin; rhou(tmapI)=rhoin*uin; rhov(tmapI)=rhoin*vin; Ener(tmapI)=Ein;

  //-------------------------------------------------------
  // Outflow conditions -- supersonic outflow ( do nothing )
  //-------------------------------------------------------

  //-------------------------------------------------------
  // Wall conditions -- reflective, isothermal, i.e., n.u=0, T=T(t=0)
  //-------------------------------------------------------
  DVec rhoW=rho(tmapW), rhouW=rhou(tmapW), rhovW=rhov(tmapW); 
  DVec nxW=nxin(tmapW), nyW=nyin(tmapW);

  // reverse flow in normal direction in ghost elements
  rhou(tmapW) = rhouW - 2.0*nxW.dm(nxW.dm(rhouW) + nyW.dm(rhovW));
  rhov(tmapW) = rhovW - 2.0*nyW.dm(nxW.dm(rhouW) + nyW.dm(rhovW));

  // NBN: conserved variables are modified in-place.
#if (0)
  // pack modified conserved variables
  Qio(All,1)=rho; Qio(All,2)=rhou; Qio(All,3)=rhov; Qio(All,4)=Ener;
#endif
}

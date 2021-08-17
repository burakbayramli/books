// CylBC2D.m
// function [rho,rhou,rhov,Ener] = CylBC2D(rho, rhou, rhov, Ener, ti)
// 2007/06/25
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedCNS2D.h"

//---------------------------------------------------------
void CurvedCNS2D::CylBC2D
(
  const DVec& rho,
  const DVec& rhou, 
  const DVec& rhov, 
  const DVec& Ener, 
        double rkti, 
        DVec& brho, 
        DVec& brhou, 
        DVec& brhov, 
        DVec& bEner
)
//---------------------------------------------------------
{
  // function [rho,rhou,rhov,Ener] = CylBC2D(rho, rhou, rhov, Ener, time)
  // Purpose: Impose channel boundary conditions on 2D CNS equations on weak form
  //
  // Example is Mach ** 0.4 ** flow in wind tunnel (gamma = 1.4)

  const DMat& gx=m_gauss.x, gy=m_gauss.y, gnx=m_gauss.nx, gny=m_gauss.ny;
  const IVec& gmapI=m_gauss.mapI, gmapW=m_gauss.mapW, gmapC=m_gauss.mapC;
  DVec  yI,rhouI,rhovI, rhoW,rhouW,rhovW,EnerW,nxW,nyW, rhoC,rhouC,rhovC,EnerC,nxC,nyC;

#if (1)
  // NBN: init all boundary nodes to current state values
  brho  = rho;
  brhou = rhou;
  brhov = rhov;
  bEner = Ener;
#endif

  //-------------------------------------------------------
  // Inflow conditions -- uniform inflow
  //-------------------------------------------------------
  double rhoin=1.4, uin=0.4, vin=0.0, pin=1.0, Temp=0.0;
  double Ein = pin/gm1 + 0.5*rhoin*(SQ(uin)+SQ(vin));

  yI = gy(gmapI); yI += 0.20;
  rhouI = rhoin * SQ(1.0/0.41)*6.0*yI.dm(0.41-yI);
  rhovI.resize(gmapI.size(),true,0.0);

  brho (gmapI) = rhoin;
  brhou(gmapI) = rhouI;
  brhov(gmapI) = rhovI;
  bEner(gmapI) = Ein + 0.5*(sqr(rhouI)+sqr(rhovI))/rhoin;


  //-------------------------------------------------------
  // Outflow conditions -- supersonic outflow ( do nothing )
  //-------------------------------------------------------


  //-------------------------------------------------------
  // Wall conditions -- reflective, isothermal, i.e., n.u=0, T=T(t=0)
  //-------------------------------------------------------
  Temp = pin/rhoin/gm1;
  rhoW=rho(gmapW); rhouW=rhou(gmapW); rhovW=rhov(gmapW); EnerW=Ener(gmapW);
  nxW=gnx(gmapW); nyW=gny(gmapW);

  brhou(gmapW) = -rhouW;
  brhov(gmapW) = -rhovW;
  bEner(gmapW) =  rhoW*Temp + 0.5*(sqr(rhouW)+sqr(rhovW)).dd(rhoW);


  //-------------------------------------------------------
  // cylinder conditions -- reflective, isothermal, i.e., n.u=0, T=T(t=0)
  //-------------------------------------------------------
  Temp = pin/rhoin/gm1;
  rhoC=rho(gmapC); rhouC=rhou(gmapC); rhovC=rhov(gmapC); EnerC=Ener(gmapC); nxC=gnx(gmapC); nyC=gny(gmapC);

  brhou(gmapC) = -rhouC;
  brhov(gmapC) = -rhovC;
  bEner(gmapC) = rhoC*Temp + 0.5*(sqr(rhouC)+sqr(rhovC)).dd(rhoC);
}

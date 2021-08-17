// CouetteBC2D.m
// function [Q] = CouetteBC2D(xin, yin, nxin, nyin, mapI, mapO, mapW, mapC, Q, time);
// 2007/06/30
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedEuler2D.h"


//---------------------------------------------------------
void CurvedEuler2D::CouetteBC2D
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
  // gmapB = concat(mapI,mapO,mapW, ...);
  // Check for no boundary faces
  if (gmapB.size() < 1) { return; }

  // function [Q] = CouetteBC2D(xin, yin, nxin, nyin, mapI, mapO, mapW, mapC, Q, time);
  // Purpose: evaluate solution for Couette flow

  // Couette flow (mach .2 at inner cylinder)
  // gamma = 1.4;

  int Nr = Qio.num_rows(); DVec rho,rhou,rhov,Ener;

  // wrap current state data (columns of Qio)
  rho.borrow (Nr, Qio.pCol(1));
  rhou.borrow(Nr, Qio.pCol(2));
  rhov.borrow(Nr, Qio.pCol(3));
  Ener.borrow(Nr, Qio.pCol(4));

  // update boundary nodes of Qio with boundary data
  // pre-calculated in function MapGaussFaceData()

  rho (gmapB) = rhoB;
  rhou(gmapB) = rhouB;
  rhov(gmapB) = rhovB;
  Ener(gmapB) = EnerB;
}

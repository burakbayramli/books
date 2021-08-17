// CouetteBC3D.cpp
// function Q = CouetteBC3D(xin, yin, zin, nxin, nyin, nzin, mapI, mapO, mapW, mapC, t, Q);
// 2007/07/11
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "Euler3D.h"


//---------------------------------------------------------
void Euler3D::CouetteBC3D
(
  const DVec& xi, 
  const DVec& yi, 
  const DVec& zi, 
  const DVec& nxi, 
  const DVec& nyi, 
  const DVec& nzi, 
  const IVec& tmapI, 
  const IVec& tmapO, 
  const IVec& tmapW, 
  const IVec& tmapC, 
        double ti, 
        DMat& Qio
)
//---------------------------------------------------------
{
  //#######################################################
  // FIXME: what about top and bottom of 3D annulus?
  //#######################################################


  // gmapB = concat(mapI,mapO,mapW, ...);
  // Check for no boundary faces
  if (gmapB.size() < 1) { return; }

  // function [Q] = CouetteBC3D(xin, yin, nxin, nyin, mapI, mapO, mapW, mapC, Q, time);
  // Purpose: evaluate solution for Couette flow

  // Couette flow (mach .2 at inner cylinder)
  // gamma = 1.4;

  int Nr = Qio.num_rows(); DVec rho,rhou,rhov,rhow,Ener;

  // wrap current state data (columns of Qio)
  rho.borrow (Nr, Qio.pCol(1));
  rhou.borrow(Nr, Qio.pCol(2));
  rhov.borrow(Nr, Qio.pCol(3));
  rhow.borrow(Nr, Qio.pCol(4));
  Ener.borrow(Nr, Qio.pCol(5));

  // update boundary nodes of Qio with boundary data
  // pre-calculated in function precalc_bdry_data()

  rho (gmapB) = rhoB;
  rhou(gmapB) = rhouB;
  rhov(gmapB) = rhovB;
  rhow(gmapB) = rhowB;
  Ener(gmapB) = EnerB;
}

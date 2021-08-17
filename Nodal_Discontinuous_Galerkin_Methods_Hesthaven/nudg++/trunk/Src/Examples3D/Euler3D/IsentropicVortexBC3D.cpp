// IsentropicVortexBC2D.m
// function Q = IsentropicVortexBC3D(xin, yin, zin, nxin, nyin, nzin, 
//                                   mapI, mapO, mapW, t, Q);
// 2007/07/11
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "Euler3D.h"


//---------------------------------------------------------
void Euler3D::IsentropicVortexBC3D
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
  // gmapB = concat(mapI,mapO,mapW);  // See Euler3D::Resize()
  if (gmapB.size() < 1) {
    // no boundary faces
    return;
  }

  //  function [Q] = IsentropicVortexBC3D(xin, yin, zin, nxin, nyin, nzin, mapI, mapO, mapW, Q, time);
  // Purpose: Impose boundary conditions on 3D Euler equations on weak form

  // Example is static isentropic vortex

  static DMat Qbc;  Qbc.resize(Qio);  DVec qn, qbcn;


  // Load Qbc with IsentropicVortex2D for time=ti
  IsentropicVortexIC3D(xi, yi, zi, ti, Qbc);

  // map isentropic vortex values onto boundary nodes
  // Note: this loop updates Qio(:,n) in-place:
  int Nr=Qio.num_rows();
  for (int n=1; n<=5; ++n) {
    qn.borrow  (Nr, Qio.pCol(n));   // qn -> Q(:,n)
    qbcn.borrow(Nr, Qbc.pCol(n));
    qn(gmapB) = qbcn(gmapB);
  }
}
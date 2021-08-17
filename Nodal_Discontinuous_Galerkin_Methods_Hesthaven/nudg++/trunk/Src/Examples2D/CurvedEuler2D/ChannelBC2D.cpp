// ChannelBC2D.cpp
// function [rho,rhou,rhov,Ener] = ChannelBC2D(rho, rhou, rhov, Ener, time)
// 2007/06/30
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedEuler2D.h"


//---------------------------------------------------------
void CurvedEuler2D::ChannelBC2D
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
  // function [rho,rhou,rhov,Ener] = ChannelBC2D(time)
  // Purpose: Impose channel boundary conditions on 2D Euler equations on weak form

  double mu = 1e-2, pbar=10.0, gamma15 = 1.5;
  DVec rho, rhou, rhov, Ener;

  // See: MapGaussFaceData()
  // gxB = gauss.x(gauss.mapB); gyB = gauss.y(gauss.mapB);

  int Nr = Qio.num_rows();
  rho.borrow (Nr, Qio.pCol(1));
  rhou.borrow(Nr, Qio.pCol(2));
  rhov.borrow(Nr, Qio.pCol(3));
  Ener.borrow(Nr, Qio.pCol(4));


  // Quadratic shear flow, relies on gamma=1.5
  rho (m_gauss.mapB) = 1.0;
  rhou(m_gauss.mapB) = sqr(gyB);
  rhov(m_gauss.mapB) = 0.0;
  Ener(m_gauss.mapB) = (2.0*mu*gxB + pbar)/(gamma15-1) + 0.5* pow(gyB,4.0);
}

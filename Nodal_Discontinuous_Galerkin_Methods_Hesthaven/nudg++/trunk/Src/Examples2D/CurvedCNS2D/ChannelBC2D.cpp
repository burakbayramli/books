// ChannelBC2D.m
// function [rho,rhou,rhov,Ener] = ChannelBC2D(rho, rhou, rhov, Ener, time)
// 2007/06/21
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedCNS2D.h"

//---------------------------------------------------------
void CurvedCNS2D::ChannelBC2D
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
  // function [rho,rhou,rhov,Ener] = ChannelBC2D(rho, rhou, rhov, Ener, time)
  // Purpose: Impose channel boundary conditions on 2D Euler equations on weak form
  //
  // Quadratic shear flow, relies on gamma=1.5

  //-------------------------------------
  // adjust parameters
  //-------------------------------------
  this->mu    = 1e-2;
  this->pbar  = 10.0;
  this->gamma = 1.5;
  this->gm1   = 0.5;  // gamma-1

  DVec xB = m_gauss.x(m_gauss.mapB);
  DVec yB = m_gauss.y(m_gauss.mapB);
  IVec gmapB = m_gauss.mapB;

  // Quadratic shear flow, relies on gamma=1.5
  brho (gmapB) = 1.0;
  brhou(gmapB) = sqr(yB);
  brhov(gmapB) = 0.0;
  bEner(gmapB) = (2.0*mu*xB + pbar)/gm1 + 0.5*pow(yB,4.0);
}

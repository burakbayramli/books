// CouetteBC2D.m
// function [Q] = CouetteBC2D(xin, yin, nxin, nyin, mapI, mapO, mapW, mapC, Q, time);
// 2007/07/11
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedCNS2D.h"


//---------------------------------------------------------
void CurvedCNS2D::CouetteBC2D
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
  // gmapB = concat(mapI,mapO,mapW, ...);
  // Check for no boundary faces
  if (gmapB.size() < 1) { return; }

  // function [Q] = CouetteBC2D(xin, yin, nxin, nyin, mapI, mapO, mapW, mapC, Q, time);
  // Purpose: evaluate solution for Couette flow

  // Couette flow (mach .2 at inner cylinder)
  // gamma = 1.4;


  // load return arrays with state data
  brho  = rho;
  brhou = rhou;
  brhov = rhov;
  bEner = Ener;

#if (1)
  // update boundary nodes with pre-calculated boundary data
  brho (gmapB) = rhoB;
  brhou(gmapB) = rhouB;
  brhov(gmapB) = rhovB;
  bEner(gmapB) = EnerB;
#else

  //-------------------------------------------------------
  // Set boundary conditions for the two cylinders
  //-------------------------------------------------------
  gmapB = concat(m_gauss.mapI, m_gauss.mapO);
  gxB = m_gauss.x(gmapB);
  gyB = m_gauss.y(gmapB);

  DVec   rad2B = sqr(gxB) + sqr(gyB);
  DVec    radB = sqrt(rad2B);
  DVec  thetaB = atan2(gyB, gxB);
  DVec uthetaB = (-radB + 16.0/radB)/75.0;
  DVec      pB = 1.0 + (1.0/SQ(75.0)) * (rad2B/2.0 - 32.0*log(radB) - 128.0/rad2B);

  // store constant boundary data
  rhoB  = ones(gmapB.size());
  rhouB = (-sin(thetaB)).dm(uthetaB);
  rhovB = ( cos(thetaB)).dm(uthetaB);
  EnerB = pB/gm1 + 0.5*(sqr(rhouB)+sqr(rhovB)).dd(rhoB);

  brho (gmapB) = rhoB;
  brhou(gmapB) = rhouB;
  brhov(gmapB) = rhovB;
  bEner(gmapB) = EnerB;


  //-------------------------------------------------------
  // Set BCs for the internal wall-circles:
  // reflective, isothermal, i.e., n.u=0, T=T(t=0)
  //-------------------------------------------------------

//const DMat& gnx=m_gauss.nx, gny=m_gauss.ny;
        DMat& gnx=m_gauss.nx, gny=m_gauss.ny;
  const IVec& gmapW=m_gauss.mapW;

  DVec gxW = m_gauss.x(gmapW);
  DVec gyW = m_gauss.y(gmapW);
  DVec   rad2W = sqr(gxW) + sqr(gyW);
  DVec    radW = sqrt(rad2W);
  DVec  thetaW = atan2(gyW, gxW);
  DVec uthetaW = (-radW + 16.0/radW)/75.0;
  DVec      pW = 1.0 + (1.0/SQ(75.0)) * (rad2W/2.0 - 32.0*log(radW) - 128.0/rad2W);


  DVec t_rhoW  = rho;
  DVec t_rhouW = rhou;
  DVec t_rhovW = rhov;
  DVec t_EnerW = Ener;

  DVec rhoW  = t_rhoW (gmapW);
  DVec rhouW = t_rhouW(gmapW);
  DVec rhovW = t_rhovW(gmapW);
  DVec EnerW = t_EnerW(gmapW);

  DVec nxW=gnx(gmapW), nyW=gny(gmapW);

  brhou(gmapW) = -rhouW;
  brhov(gmapW) = -rhovW;
  bEner(gmapW) =  pW/gm1 + 0.5*(sqr(rhouW)+sqr(rhovW)).dd(rhoW);


#endif
}

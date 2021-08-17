// PearsonVortexBC2D.m
// function [bcUx, bcUy, bcPR, bcdUndt] = 
//   PearsonVortexBC2D(x, y, nx, ny, mapI, mapO, mapW, mapC, time, nu)
// 2007/06/16
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedINS2D.h"


//---------------------------------------------------------
void CurvedINS2D::PearsonVortexBC2D
(
  const DVec&   xin,    // [in]
  const DVec&   yin,    // [in]
  const DVec&   nxi,    // [in]
  const DVec&   nyi,    // [in]
  const IVec&   MAPI,   // [in]
  const IVec&   MAPO,   // [in]
  const IVec&   MAPW,   // [in]
  const IVec&   MAPC,   // [in]
        double  ti,     // [in]
        double  nu,     // [in]
        DVec&   BCUX,   // [out]
        DVec&   BCUY,   // [out]
        DVec&   BCPR,   // [out]
        DVec&   BCDUNDT // [out]
)
//---------------------------------------------------------
{
  // function [bcUx, bcUy, bcPR, bcdUdn] = PearsonVortexBC2D(x, y, nx, ny, mapI, mapO, mapW, time, nu)
  // Purpose: evaluate boundary conditions for Pearson's vortex exact solution 

  static DVec  xI( "xI"),  yI( "yI"),  xO( "xO"),  yO( "yO");
  static DVec nxI("nxI"), nyI("nyI"), nxO("nxO"), nyO("nyO");

  int len = xin.size();
  BCUX.resize(len); BCUY.resize(len);     // resize result arrays
  BCPR.resize(len); BCDUNDT.resize(len);  // and set to zero

  ti = 0;

  double en4pit = exp(-nu*4.0*SQ(pi)*ti);
  double en8pit = exp(-nu*8.0*SQ(pi)*ti);
  double twopi = 2.0*pi;

  // Dirichlet velocity, condition on inflow
  xI = xin(mapI); yI = yin(mapI); nxI = nxi(mapI); nyI = nyi(mapI);
  
  BCUX(mapI)= -sin(twopi*yI) * en4pit;
  BCUY(mapI)=  sin(twopi*xI) * en4pit;
#if (1)
  // NBN: original was the other way
  BCPR(mapI) = (-cos(twopi*xI)).dm(cos(twopi*yI)) * en8pit;
#else
  BCDUNDT(mapI) = ( -nxI.dm(sin(twopi*yI)) + nyI.dm(sin(twopi*xI)) ) * en4pit;
#endif

  // Neumann velocity, Dirichlet pressure, condition on outflow
  xO = xin(mapO); yO = yin(mapO); nxO = nxi(mapO); nyO = nyi(mapO);
  
  BCUX(mapO) = nyO.dm( twopi * (-cos(twopi*yO) * en4pit) );
  BCUY(mapO) = nxO.dm( twopi * ( cos(twopi*xO) * en4pit) );
  BCPR(mapO) = (-cos(twopi*xO)).dm(cos(twopi*yO)) * en8pit;
}



// INScouetteBC2D.cpp
// function [bcUx, bcUy, bcPR, bcdUndt] = 
//   INScouetteBC2D(x, y, nx, ny, mapI, mapO, mapW, mapC, time, nu)
// 2007/07/14
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedINS2D.h"


//---------------------------------------------------------
void CurvedINS2D::INScouetteBC2D
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
  // function [bcUx, bcUy, bcPR, bcdUdn] = INScouetteBC2D(x, y, nx, ny, mapI, mapO, mapW, time, nu)
  // Purpose: evaluate boundary conditions for Taylor-Couette 
  //          flow with inner/outer cylinders at {r=1,r=4}

  int len = xin.size();
  BCUX.resize(len); BCUY.resize(len);     // resize result arrays
  BCPR.resize(len); BCDUNDT.resize(len);  // and set to zero

  // set boundary nodes to pre-calculated exact (inital) solution

#if (0)
  BCUX(gmapB) = uB;
  BCUY(gmapB) = vB;
  BCPR(gmapB) = pB;
//BCDUNDT(gmapB) = 0.0;
#else

  gmapB = concat(MAPI,MAPO,MAPW);
  gxB = xin(gmapB); gyB = yin(gmapB);

  DVec   rad2B = sqr(gxB) + sqr(gyB);
  DVec    radB = sqrt(rad2B);
  DVec  thetaB = atan2(gyB, gxB);
  DVec uthetaB = (-radB + 16.0/radB)/75.0;

  // store constant boundary data
  uB = (-sin(thetaB)).dm(uthetaB);
  vB = ( cos(thetaB)).dm(uthetaB);
  pB = 1.0 + (1.0/SQ(75.0)) * (rad2B/2.0 - 32.0*log(radB) - 128.0/rad2B);

  BCUX(gmapB) = uB;
  BCUY(gmapB) = vB;
  BCPR(gmapB) = pB;

#endif

}

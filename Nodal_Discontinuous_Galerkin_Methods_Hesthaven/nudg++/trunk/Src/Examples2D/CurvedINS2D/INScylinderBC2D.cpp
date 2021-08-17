// INScylinderBC2D.m
// function [bcUx, bcUy, bcPR, bcdUndt] = 
//    INScylinderBC2D(x, y, nx, ny, mapI, mapO, mapW, mapC, time, nu)
// 2007/06/10
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedINS2D.h"


//---------------------------------------------------------
void CurvedINS2D::INScylinderBC2D
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
  // function [bcUx, bcUy, bcPR, bcdUndt] = INScylinderBC2D(x, y, nx, ny, mapI, mapO, mapW, mapC, time, nu)
  // Purpose: evaluate boundary conditions for channel bounded cylinder flow with walls at y=+/- .15

  // TEST CASE: from 
  // V. John "Reference values for drag and lift of a two-dimensional time dependent flow around a cylinder", 
  // Int. J. Numer. Meth. Fluids 44, 777 - 788, 2004

  DVec yI("yI");  int len = xin.size();
  BCUX.resize(len); BCUY.resize(len);     // resize result arrays
  BCPR.resize(len); BCDUNDT.resize(len);  // and set to zero

  // inflow
#ifdef _MSC_VER
  yI = yin(MAPI);  yI += 0.20;
#else
  int Ni=MAPI.size(); yI.resize(Ni);
  for(int n=1;n<=Ni;++n) {yI(n)=yin(MAPI(n))+0.2;}
#endif

  BCUX(MAPI)    =  SQ(1.0/0.41)*6.0 * yI.dm(0.41 - yI);
  BCUY(MAPI)    =  0.0;
  BCDUNDT(MAPI) = -SQ(1.0/0.41)*6.0 * yI.dm(0.41 - yI);

  // wall
  BCUX(MAPW) = 0.0;
  BCUY(MAPW) = 0.0;

  // cylinder
  BCUX(MAPC) = 0.0;
  BCUY(MAPC) = 0.0;

  // outflow
  BCUX(MAPO)    = 0.0;
  BCUY(MAPO)    = 0.0;
  BCDUNDT(MAPO) = 0.0;
}

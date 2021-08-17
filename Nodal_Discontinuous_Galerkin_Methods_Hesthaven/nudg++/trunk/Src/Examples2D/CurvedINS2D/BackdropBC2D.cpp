// BackdropBC2D.cpp
// function [bcUx, bcUy, bcPR, bcdUndt] = 
//    BackdropBC2D(x, y, nx, ny, mapI, mapO, mapW, mapC, time, nu)
// 2007/07/23
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedINS2D.h"


//---------------------------------------------------------
void CurvedINS2D::BackdropBC2D
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
  // function [bcUx, bcUy, bcPR, bcdUndt] = BackdropBC2D(x, y, nx, ny, mapI, mapO, mapW, mapC, time, nu)
  // Purpose: evaluate boundary conditions for "Backdrop" configuration

  int len = xin.size();

  BCUX.resize(len); BCUY.resize(len);     // resize result arrays
  BCPR.resize(len); BCDUNDT.resize(len);  // and set to zero

  // enforce inflow boundary conditions
  BCUX(MAPI) =  1.0;
  BCUY(MAPI) =  0.0;

#if (1)
  // ... ??
  BCDUNDT(MAPI) = 0.0;
#endif
}

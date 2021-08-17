// KovasznayBC2D.m
// function [bcUx, bcUy, bcPR, bcdUndt] = 
//    KovasznayBC2D(x, y, nx, ny, MAPI, MAPO, MAPW, MAPC, time, nu)
// 2007/06/15
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedINS2D.h"


//---------------------------------------------------------
void CurvedINS2D::KovasznayBC2D
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
  // function [bcUx, bcUy, bcPR, bcdUndt] = KovasznayBC2D(x, y, nx, ny, MAPI, MAPO, MAPW, MAPC, time, nu)
  // Purpose: evaluate boundary conditions for Kovasznay flow 

  static DVec xI("xI"), yI("yI"), xO("xO"), yO("yO");

  int len = xin.size();
  BCUX.resize(len); BCUY.resize(len);     // resize result arrays
  BCPR.resize(len); BCDUNDT.resize(len);  // and set to zero

  double lam = (0.5/nu) - sqrt( (0.25/SQ(nu)) + 4.0*SQ(pi));

  // inflow
#ifdef _MSC_VER
  xI = xin(MAPI);  yI = yin(MAPI);
#else
  int Ni=MAPI.size(), n=0; xI.resize(Ni); yI.resize(Ni);
  for (n=1;n<=Ni;++n) {xI(n)=xin(MAPI(n));}
  for (n=1;n<=Ni;++n) {yI(n)=yin(MAPI(n));}
#endif

  DVec elamXI = exp(lam*xI), twopiyI = 2.0*pi*yI;

  BCUX(MAPI) =          1.0 - elamXI.dm(cos(twopiyI));
  BCUY(MAPI) = (0.5*lam/pi) * elamXI.dm(sin(twopiyI));


  // outflow
#ifdef _MSC_VER
  xO = xin(MAPO); yO = yin(MAPO);
#else
  int No=MAPO.size(); xO.resize(No); yO.resize(No);
  for (n=1;n<=No;++n) {xO(n)=xin(MAPO(n));}
  for (n=1;n<=No;++n) {yO(n)=yin(MAPO(n));}
#endif

  DVec elamXO = exp(lam*xO), twopiyO = 2.0*pi*yO;

  BCPR   (MAPO) =  0.5*(1.0-exp(2.0*lam*xO));
//BCDUNDT(MAPO) = -lam*elamXO.dm(cos(twopiyO));

  if (0) {
    // THIS WORKED
    BCUX(MAPO) =         1.0  - elamXO.dm(cos(twopiyO));
    BCUY(MAPO) = (0.5*lam/pi) * elamXO.dm(sin(twopiyO));
  } else {
    // Neumann data for each velocity
    BCUX(MAPO) = -lam*               elamXO.dm(cos(twopiyO));
    BCUY(MAPO) =  lam*(0.5*lam/pi) * elamXO.dm(sin(twopiyO));
  }
}

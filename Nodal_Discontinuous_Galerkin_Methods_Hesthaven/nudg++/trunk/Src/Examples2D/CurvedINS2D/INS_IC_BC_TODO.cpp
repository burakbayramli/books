// Stubbs for CurvedINS
// initial and boundary conditions 
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedINS2D.h"

// Channel:
//---------------------------------------------------------
void CurvedINS2D::INSchannelIC2D
(
  const DVec&   xi, 
  const DVec&   yi, 
        double  ti,   // [in]
        double  nu,   // [in]
        DMat&   Uxo,  // [out]
        DMat&   Uyo,  // [out]
        DMat&   PRo   // [out]
)
//---------------------------------------------------------
{
  umERROR("CurvedINS2D::INSchannelIC2D", "Not yet implemented");
}


//---------------------------------------------------------
void CurvedINS2D::INSchannelBC2D
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
  umERROR("CurvedINS2D::INSchannelBC2D", "Not yet implemented");
}

// BackdropIC2D.cpp
// function [Ux, Uy, PR] = BackdropIC2D(x, y, time, nu)
// 2007/07/23
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedINS2D.h"

//---------------------------------------------------------
void CurvedINS2D::BackdropIC2D
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
  // function [Ux, Uy, PR] = BackdropIC2D(x, y, time, nu)
  // Purpose: evaluate solution for "Backdrop" configuration

  Uxo.fill(0.0);
  Uyo.fill(0.0);
  PRo.fill(0.0);
}


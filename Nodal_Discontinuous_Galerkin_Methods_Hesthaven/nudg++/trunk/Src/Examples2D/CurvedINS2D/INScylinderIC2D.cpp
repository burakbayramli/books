// INScylinderIC2D.m
// function [Ux, Uy, PR] = INScylinderIC2D(x, y, time, nu)
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedINS2D.h"

//---------------------------------------------------------
void CurvedINS2D::INScylinderIC2D
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
  // function [Ux, Uy, PR] = INScylinderIC2D(x, y, time, nu)
  // Purpose: evaluate solution for channel bounded 
  // cylinder flow with walls at y = +/- 0.15

  Uxo.fill(0.0);
  Uyo.fill(0.0);
  PRo.fill(0.0);
}

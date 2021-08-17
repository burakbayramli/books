// KovasznayIC2D.m
// function [Ux, Uy, PR] = INScylinderIC2D(x, y, time, nu)
// 2007/06/15
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedINS2D.h"

//---------------------------------------------------------
void CurvedINS2D::KovasznayIC2D
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
  // function [Ux, Uy, PR] = KovasznayIC2D(x, y, time, nu)
  // Purpose: evaluate solution for Kovasznay flow 

  double lam = (0.5/nu) - sqrt( (0.25/SQ(nu)) + 4.0*SQ(pi));
  DVec elamX = exp(lam*xi), twopiy = 2.0*pi*yi;

  Uxo =          1.0 - elamX.dm(cos(twopiy));
  Uyo = (0.5*lam/pi) * elamX.dm(sin(twopiy));
  PRo =  0.5 *  (1.0 - exp(2.0*lam*xi));
}

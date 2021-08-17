// PearsonVortexIC2D.m
// function [Ux, Uy, PR] = PearsonVortexIC2D(x, y, time, nu)
// 2007/06/16
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedINS2D.h"

//---------------------------------------------------------
void CurvedINS2D::PearsonVortexIC2D
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
  // function [Ux, Uy, PR] = PearsonVortexIC2D(x, y, time, nu)
  // Purpose: evaluate solution for channel flow with walls at y=0,2

  Uxo = -sin(2.0*pi*y) * exp(-nu*4.0*SQ(pi)*ti);
  Uyo =  sin(2.0*pi*x) * exp(-nu*4.0*SQ(pi)*ti);
  PRo = -cos(2.0*pi*x).dm(cos(2.0*pi*y)) * exp(-nu*8.0*SQ(pi)*ti);
}
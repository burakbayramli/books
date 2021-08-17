// Grad2D.m
// function [ux,uy] = Grad2D(u);
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"


//---------------------------------------------------------
void NDG2D::Grad2D
(
  const DMat& u,    // [in]
        DMat& ux,   // [out]
        DMat& uy    // [out]
)
//---------------------------------------------------------
{
  // function [ux,uy] = Grad2D(u);
  // Purpose: Compute 2D gradient field of scalar u

  DVec ur = Dr*u, us = Ds*u;
  ux = rx.dm(ur) + sx.dm(us); uy = ry.dm(ur) + sy.dm(us);
}

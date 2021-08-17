// xytors.m
// function [r,s] = xytors(x,y)
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"


//---------------------------------------------------------
void xytors
(
  const DVec& x,  // [in]
  const DVec& y,  // [in]
        DVec& r,  // [out]
        DVec& s   // [out]
)
//---------------------------------------------------------
{
  // function [r,s] = xytors(x,y)
  // Purpose : Transfer from (x,y) in equilateral triangle
  //           to (r,s) coordinates in standard triangle
  DVec L1,L2,L3;

  L1 =          (sqrt(3.0)*y + 1.0)/3.0;
  L2 = (-3.0*x - sqrt(3.0)*y + 2.0)/6.0;
  L3 = ( 3.0*x - sqrt(3.0)*y + 2.0)/6.0;

  r = -L2 + L3 - L1;  s = -L2 - L3 + L1;
}

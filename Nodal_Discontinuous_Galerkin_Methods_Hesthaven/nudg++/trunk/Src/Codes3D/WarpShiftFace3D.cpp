// WarpShiftFace3D.m
// function [warpx, warpy] = WarpShiftFace3D(p,pval, pval2, L1,L2,L3,L4)
// 2007/06/11
//---------------------------------------------------------
#include "NDGLib_headers.h"


//---------------------------------------------------------
void WarpShiftFace3D
(
  int p,
  double pval, 
  double pval2, 
  const DVec& L1,
  const DVec& L2,
  const DVec& L3,
  const DVec& L4,
        DVec& warpx, 
        DVec& warpy
)
//---------------------------------------------------------
{
  // function [warpx, warpy] = WarpShiftFace3D(p,pval, pval2, L1,L2,L3,L4)     
  // Purpose: compute warp factor used in creating 3D Warp & Blend nodes

  DVec dtan1,dtan2; 
  evalshift(p, pval, L2, L3, L4,  dtan1,dtan2);
  warpx = dtan1;
  warpy = dtan2;
}

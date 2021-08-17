// evalshift.m
// function [dx, dy] = evalshift(p, pval, L1, L2, L3)  
// 2007/06/11
//---------------------------------------------------------
#include "NDGLib_headers.h"


//---------------------------------------------------------
void evalshift
(
  int p, 
  double pval, 
  const DVec& L1, 
  const DVec& L2, 
  const DVec& L3,
        DVec& dx, 
        DVec& dy
)
//---------------------------------------------------------
{
  // function [dx, dy] = evalshift(p, pval, L1, L2, L3)  
  // Purpose: compute two-dimensional Warp & Blend transform

  // 1) compute Gauss-Lobatto-Legendre node distribution
  DVec gaussX = -JacobiGL(0,0,p);  DVec warp1,warp2,warp3;
   
  // 3) compute blending function at each node for each edge
  DVec blend1 = L2.dm(L3);
  DVec blend2 = L1.dm(L3);
  DVec blend3 = L1.dm(L2);

  // 4) amount of warp for each node, for each edge
  DVec tv1=L3-L2, tv2=L1-L3, tv3=L2-L1;  // cannot pass OBJ_temp
  DVec warpfactor1 = 4.0 * evalwarp(p, gaussX, tv1);
  DVec warpfactor2 = 4.0 * evalwarp(p, gaussX, tv2);
  DVec warpfactor3 = 4.0 * evalwarp(p, gaussX, tv3);

  // 5) combine blend & warp
  warp1 = blend1.dm(warpfactor1);  warp1 *= (1.0 + sqr(pval*L1));
  warp2 = blend2.dm(warpfactor2);  warp2 *= (1.0 + sqr(pval*L2));
  warp3 = blend3.dm(warpfactor3);  warp3 *= (1.0 + sqr(pval*L3));

  // 6) evaluate shift in equilateral triangle
  dx = 1.0*warp1 + cos(TWOPI/3.0)*warp2 + cos(FOURPI/3.0)*warp3;
  dy = 0.0*warp1 + sin(TWOPI/3.0)*warp2 + sin(FOURPI/3.0)*warp3;
}

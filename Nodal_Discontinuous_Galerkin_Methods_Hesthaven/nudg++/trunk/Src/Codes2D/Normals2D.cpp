// Normals2D.m
// function [nx, ny, sJ] = Normals2D()
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"


//---------------------------------------------------------
void NDG2D::Normals2D()  // [nx, ny, sJ]
//---------------------------------------------------------
{
  // function [nx, ny, sJ] = Normals2D()
  // Purpose : Compute outward pointing normals at
  //	    elements faces as well as surface Jacobians

  DMat xr=Dr*x, yr=Dr*y, xs=Ds*x, ys=Ds*y;
  this->J = xr.dm(ys) - xs.dm(yr);

  // interpolate geometric factors to face nodes
  DMat fxr = xr(Fmask, All), fxs = xs(Fmask, All),
       fyr = yr(Fmask, All), fys = ys(Fmask, All);

  // build normals
  nx.resize(3*Nfp, K); ny.resize(3*Nfp, K);
  Index1D fid1(1,Nfp), fid2(Nfp+1,2*Nfp), fid3(2*Nfp+1,3*Nfp);

  // face 1
  nx(fid1, All) =  fyr(fid1, All);
  ny(fid1, All) = -fxr(fid1, All);

  // face 2
  nx(fid2, All) =  fys(fid2, All)-fyr(fid2, All);
  ny(fid2, All) = -fxs(fid2, All)+fxr(fid2, All);

  // face 3
  nx(fid3, All) = -fys(fid3, All); 
  ny(fid3, All) =  fxs(fid3, All);

  // normalise
  sJ = sqrt(sqr(nx)+sqr(ny));  // nx=nx.dd(sJ); ny=ny.dd(sJ);
  nx.div_element(sJ); ny.div_element(sJ);
}

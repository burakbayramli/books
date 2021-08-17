// Normals3D.m
// function [nx, ny, nz, sJ] = Normals3D()
// 2007/06/11
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG3D.h"


//---------------------------------------------------------
void NDG3D::Normals3D()
//---------------------------------------------------------
{
  // function [nx, ny, nz, sJ] = Normals3D()
  // Purpose : Compute outward pointing normals at
  //	    elements faces as well as surface Jacobians

  GeometricFactors3D();

  // interpolate geometric factors to face nodes
  DMat frx=rx(Fmask,All),  fsx=sx(Fmask,All),  ftx=tx(Fmask,All);
  DMat fry=ry(Fmask,All),  fsy=sy(Fmask,All),  fty=ty(Fmask,All);
  DMat frz=rz(Fmask,All),  fsz=sz(Fmask,All),  ftz=tz(Fmask,All);

  // build normals
  nx.resize(4*Nfp, K); ny.resize(4*Nfp, K); nz.resize(4*Nfp, K);
  Index1D fid1(1,Nfp), fid2(Nfp+1,2*Nfp), fid3(2*Nfp+1,3*Nfp), fid4(3*Nfp+1,4*Nfp);

  // face 1
  nx(fid1, All) = -ftx(fid1,All);
  ny(fid1, All) = -fty(fid1,All);
  nz(fid1, All) = -ftz(fid1,All);

  // face 2
  nx(fid2, All) = -fsx(fid2,All);
  ny(fid2, All) = -fsy(fid2,All);
  nz(fid2, All) = -fsz(fid2,All);

  // face 3
  nx(fid3, All) = frx(fid3,All) + fsx(fid3,All) + ftx(fid3,All);
  ny(fid3, All) = fry(fid3,All) + fsy(fid3,All) + fty(fid3,All);
  nz(fid3, All) = frz(fid3,All) + fsz(fid3,All) + ftz(fid3,All);

  // face 4
  nx(fid4, All) = -frx(fid4,All);
  ny(fid4, All) = -fry(fid4,All);
  nz(fid4, All) = -frz(fid4,All);

  // normalise
  sJ = sqrt(sqr(nx) + sqr(ny) + sqr(nz));
  
  nx.div_element(sJ); ny.div_element(sJ); nz.div_element(sJ);
  sJ.mult_element(J(Fmask, All));  //sJ=sJ.*J(Fmask(:),:);
}

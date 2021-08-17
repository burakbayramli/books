// FindLocalCoords2D.m
// function [rOUT,sOUT] = FindLocalCoords2D(k, xout, yout)
// 2007/07/27
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"


//---------------------------------------------------------
void NDG2D::FindLocalCoords2D
(
  int k, 
  const DVec& xout, 
  const DVec& yout, 
        DVec& rOUT,
        DVec& sOUT
)
//---------------------------------------------------------
{
  // function [rOUT,sOUT] = FindLocalCoords2D(k, xout, yout)
  // purpose: find local (r,s) coordinates in the k'th element of given coordinates
  //          [only works for straight sided triangles]

  DVec xy1(2), xy2(2), xy3(2), xy(2), tmp(2), rhs;
  DMat A(2,2); int v1=0, v2=0, v3=0;
   
  v1=EToV(k,1); v2=EToV(k,2); v3=EToV(k,3);

  xy1(1)=VX(v1);  xy1(2)=VY(v1);
  xy2(1)=VX(v2);  xy2(2)=VY(v2);
  xy3(1)=VX(v3);  xy3(2)=VY(v3);

  A(All,1) = xy2-xy1;  A(All,2) = xy3-xy1;

  int len = xout.size();
  rOUT.resize(len);  sOUT.resize(len);
  for (int i=1; i<=len; ++i) 
  {
    xy(1)=xout(i); xy(2)=yout(i);
    rhs = 2.0*xy - xy2 - xy3;
    tmp = A|rhs;
    rOUT(i) = tmp(1);
    sOUT(i) = tmp(2);
  }
}

// xyztorst.m
// function [r, s, t] = xyztorst(X, Y, Z)
// 2007/06/12
//---------------------------------------------------------
#include "NDGLib_headers.h"


//---------------------------------------------------------
void xyztorst
(
  const DVec& X,  // [in]
  const DVec& Y,  // [in]
  const DVec& Z,  // [in]
        DVec& r,  // [out]
        DVec& s,  // [out]
        DVec& t   // [out]
)
//---------------------------------------------------------
{
  // function [r,s,t] = xyztorst(x, y, z)
  // Purpose : Transfer from (x,y,z) in equilateral tetrahedron
  //           to (r,s,t) coordinates in standard tetrahedron

  double sqrt3=sqrt(3.0), sqrt6=sqrt(6.0); int Nc=X.size();
  DVec v1(3),v2(3),v3(3),v4(3);
  DMat tmat1(3,Nc), A(3,3), rhs; 
  
  v1(1)=(-1.0);  v1(2)=(-1.0/sqrt3);  v1(3)=(-1.0/sqrt6);
  v2(1)=( 1.0);  v2(2)=(-1.0/sqrt3);  v2(3)=(-1.0/sqrt6);
  v3(1)=( 0.0);  v3(2)=( 2.0/sqrt3);  v3(3)=(-1.0/sqrt6);
  v4(1)=( 0.0);  v4(2)=( 0.0      );  v4(3)=( 3.0/sqrt6);

  // back out right tet nodes
  tmat1.set_row(1,X); tmat1.set_row(2,Y); tmat1.set_row(3,Z);
  rhs = tmat1 - 0.5*outer(v2+v3+v4-v1, ones(Nc));
  A.set_col(1,0.5*(v2-v1)); A.set_col(2,0.5*(v3-v1)); A.set_col(3,0.5*(v4-v1));

  DMat RST = A|rhs;

  r=RST.get_row(1); s=RST.get_row(2); t=RST.get_row(3);
}

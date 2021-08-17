// Nodes3D.m
// function [X,Y,Z] = Nodes3D(p)
// 2007/06/12
//---------------------------------------------------------
#include "NDGLib_headers.h"


//---------------------------------------------------------
void Nodes3D(int p, DVec& X, DVec& Y, DVec& Z)
//---------------------------------------------------------
{
  // function [X,Y,Z] = Nodes3D(p)
  // Purpose: compute Warp & Blend nodes
  //   input: p = polynomial order of interpolant
  //  output: X,Y,Z vectors of node coordinates in equilateral tetrahedron

  // choose optimized blending parameter
  DVec alphastore(gVecData, 15, 
        " 0.0000  0.0000  0.0000  0.1002  1.1332  " 
        " 1.5608  1.3413  1.2577  1.1603  1.10153 "
        " 0.6080  0.4523  0.8856  0.8717  0.9655  ");

  double alpha = 1.0;
  if (p<=15) { alpha = alphastore(p); }

  // total number of nodes and tolerance
  // int N = (p+1)*(p+2)*(p+3)/6;
  double tol=1e-10, sqrt3=sqrt(3.0), sqrt6=sqrt(6.0);
  DVec r("r"),s("s"),t("t"), v1(3),v2(3),v3(3),v4(3), r1,r2;
  DVec L1,L2,L3,L4, La,Lb,Lc,Ld, warp1, warp2, blend, denom, tv1,tv2; 
  IVec ids,ids1,ids2; DMat t1(4,3), t2(4,3), XYZ, shift;

  EquiNodes3D(p, r,s,t); // create equidistributed nodes 
  L1=(1.0+t)/2.0; L2=(1.0+s)/2.0; L3=(-(1.0+r+s+t)/2.0); L4=(1.0+r)/2.0;

  // set vertices of tetrahedron
  v1(1)=(-1.0);  v1(2)=(-1.0/sqrt3);  v1(3)=(-1.0/sqrt6);
  v2(1)=( 1.0);  v2(2)=(-1.0/sqrt3);  v2(3)=(-1.0/sqrt6);
  v3(1)=( 0.0);  v3(2)=( 2.0/sqrt3);  v3(3)=(-1.0/sqrt6);
  v4(1)=( 0.0);  v4(2)=( 0.0      );  v4(3)=( 3.0/sqrt6);


  // orthogonal axis tangents on faces 1-4
  t1(1,All) = v2-v1;          t1(2,All) = v2-v1;
  t1(3,All) = v3-v2;          t1(4,All) = v3-v1;   
  t2(1,All) = v3-0.5*(v1+v2); t2(2,All) = v4-0.5*(v1+v2);
  t2(3,All) = v4-0.5*(v2+v3); t2(4,All) = v4-0.5*(v1+v3);  


  // normalize tangents 
  for (int n=1; n<=4; ++n) {
    r1 = t1.get_row(n);  t1.set_row(n, r1/r1.norm());
    r2 = t2.get_row(n);  t2.set_row(n, r2/r2.norm());
  }


  // Warp and blend for each face (accumulated in shiftXYZ)
  // form undeformed coordinates
  XYZ = outer(L3,v1) + outer(L4,v2) + outer(L2,v3) + outer(L1,v4);
  shift.resize(XYZ);

  for (int face=1; face<=4; ++face) {

    if      (1==face) { La = L1; Lb = L2; Lc = L3; Ld = L4; }
    else if (2==face) { La = L2; Lb = L1; Lc = L3; Ld = L4; }
    else if (3==face) { La = L3; Lb = L1; Lc = L4; Ld = L2; }
    else if (4==face) { La = L4; Lb = L1; Lc = L3; Ld = L2; }

    // compute warp tangential to face
    WarpShiftFace3D(p, alpha, alpha, La, Lb, Lc, Ld, warp1, warp2); 

    blend = Lb.dm(Lc); blend *= Ld;   // compute volume blending

  //denom = (Lb+0.5*La)       .*(Lc+0.5*La)       .*(Ld+0.5*La); // modify linear blend
    denom = (Lb+0.5*La); denom*=(Lc+0.5*La); denom*=(Ld+0.5*La); // modify linear blend

    ids = find(denom, '>', tol);
  //blend(ids) = (1.0+(alpha*La(ids)).^2) .* blend(ids)./ denom(ids);

    tv1 = 1.0+sqr(alpha*La(ids)); tv2 = dd(blend(ids), denom(ids));
    blend(ids) = tv1.dm(tv2);

    // compute warp & blend
    shift += outer( blend.dm(warp1), t1.get_row(face) ) + 
             outer( blend.dm(warp2), t2.get_row(face) );

    // fix face warp 
  //ids = find(La<tol & ( (Lb>tol) + (Lc>tol) + (Ld>tol) < 3));
    ids1 = find( La, '<', tol);
    ids2 = find((Lb.gt(tol)+Lc.gt(tol)+Ld.gt(tol)), '<', 3.0);
    ids  = find(ids1, '&', ids2);

  //shift(ids,All) = outer(warp1(ids), t1.get_row(face)) + 
  //                 outer(warp2(ids), t2.get_row(face));

    DVec tv1a = warp1(ids), tv1b = t1.get_row(face);
    DVec tv2a = warp2(ids), tv2b = t2.get_row(face);
    shift(ids,All) = outer(tv1a, tv1b) + outer(tv2a, tv2b);
  }

  XYZ += shift;
  X = XYZ(All,1); Y = XYZ(All,2); Z = XYZ(All,3);
}

// IntersectTest3D.cpp
// function [retval,poly] =  IntersectTest3D(x1, x2)
// 2007/10/10
//---------------------------------------------------------
#include "NDGLib_headers.h"

#include "Poly3D.h"


// returns true if polygons intersect
//---------------------------------------------------------
bool IntersectTest3D(const DM& X1, const DM& X2, Poly3D& poly)
//---------------------------------------------------------
{
  bool retval = false;
  
  int Nverts=3, i=0,j=0; DVec point,v1,v2,v3;

  // make "non-const" to allow "borrow"
  DM& x1 = const_cast<DM&>(X1);
  DM& x2 = const_cast<DM&>(X2);

  // Clear any existing polynomial
  poly.Clear();

  // find nodes of triangle 1 which lie in triangle 2
  for (i=1; i<=Nverts; ++i) {
    point.borrow(3, x1.pCol(i));
    if (PointInTri3D(x2, point)) {
      poly.AddPoint(point);
    }
  }

  // triangle 1 is totally inside triangle 2
  if (3 == poly.m_N) {

    v1.borrow(3, x1.pCol(1));
    v2.borrow(3, x1.pCol(2));
    v3.borrow(3, x1.pCol(3));

    poly.AddTriangle(v1, v2, v3);
    return true;
  }

  // find nodes of triangle 2 which lie in triangle 1
  int cnt1 = 0;
  for (i=1; i<=Nverts; ++i) {
    point.borrow(3, x2.pCol(i));
    if (PointInTri3D(x1, point)) {
      poly.AddPoint(point);
      ++cnt1;
    }
  }

  // triangle 2 is totally inside 1
  if (3 == cnt1) {

    v1.borrow(3, x2.pCol(1));
    v2.borrow(3, x2.pCol(2));
    v3.borrow(3, x2.pCol(3));

    poly.AddTriangle(v1, v2, v3);
    return true;
  }

  // now check  edge intersections
  DVec res(3), vert11,vert12,vert21,vert22; 
  int cnt2=0, idx=0,jdx=0; bool btest=false;
  for (i=1; i<=Nverts; ++i) {
    for (j=1; j<=Nverts; ++j) {
      idx = 1+(i%Nverts); jdx = 1+(j%Nverts);
      vert11.borrow(3, x1.pCol(i));  vert12.borrow(3, x1.pCol(idx));
      vert21.borrow(3, x2.pCol(j));  vert22.borrow(3, x2.pCol(jdx));
      btest = EdgeIntersect3D(vert11, vert12, vert21, vert22, res);
      if (btest) {
        poly.AddPoint(res);
        ++cnt2;
      }
    }
  }

  if (poly.m_N <=2) {
    return false;       // There is no 2 dimensional intersection
  } 
  else if (3 == poly.m_N)
  {
    v1.borrow(3, poly.m_xyz.pCol(1));
    v2.borrow(3, poly.m_xyz.pCol(2));
    v3.borrow(3, poly.m_xyz.pCol(3));
    poly.AddTriangle(v1, v2, v3); // There is one triangle
    return true;
  }
  else 
  {
    poly.BuildTriangles();  // There is more than one triangle
    return true;
  }
}


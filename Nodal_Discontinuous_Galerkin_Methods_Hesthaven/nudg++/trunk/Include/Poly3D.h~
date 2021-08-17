// Poly3D.h
// 
// 2007/10/18
//---------------------------------------------------------
#ifndef NDG__Poly3D_H__INCLUDED
#define NDG__Poly3D_H__INCLUDED


// #define FAST_POLY3D 1

#include "Mat_Col.h"
#include "MatObj_Type.h"
#include "VecObj_Type.h"


//---------------------------------------------------------
class Poly3D 
//---------------------------------------------------------
{
public:
  Poly3D();
  Poly3D(const Poly3D& p);
  ~Poly3D();

  Poly3D& operator=(const Poly3D& p);

  void Clear();
  bool HavePoint(const DVec& point);
  void AddPoint(const DVec& point);
  void AddTriangle(const DVec& v1, const DVec& v2, const DVec& v3);
  void BuildTriangles();
  void SortPoints(const DVec& cent);

public:
  int m_N, m_Ntri;
  DM  m_xyz, m_x, m_y, m_z;
  DV  m_areas;
  static int s_Npolys;
};


// helper routines for class Poly3D (see file Poly3D.cpp)

bool    IntersectTest3D (const DM& x1, const DM& x2, Poly3D& poly);
bool    PointInTri3D    (const DM& tri, const DVec& point);
double  TriArea3D       (const DVec& Xa, const DVec& Xb, const DVec& Xc);
bool    EdgeIntersect3D (const DVec& a1, const DVec& b1, const DVec& a2, const DVec& b2, DVec& xint);


#endif  // NDG__Poly3D_H__INCLUDED

// Poly3D.cpp
// utility class for polygons in 3D 
// 2007/10/17
//---------------------------------------------------------
#include "NDGLib_headers.h"

#include "Poly3D.h"

int Poly3D::s_Npolys=0;

//---------------------------------------------------------
Poly3D::Poly3D() : m_N(0), m_Ntri(0) 
//---------------------------------------------------------
{
  ++s_Npolys;

#ifndef NDEBUG
  umTRC(1, "++Poly3D: %6d\n", s_Npolys);
#endif
}


//---------------------------------------------------------
Poly3D::~Poly3D() 
//---------------------------------------------------------
{
  --s_Npolys;

#ifndef NDEBUG
  umTRC(1, "--Poly3D: %6d\n", s_Npolys);
#endif
}


//---------------------------------------------------------
Poly3D::Poly3D(const Poly3D& p)
//---------------------------------------------------------
{
  ++s_Npolys;

#ifndef NDEBUG
  umTRC(1, "++Poly3D: %6d (copy ctor)\n", s_Npolys);
#endif

  (*this) = p;
}


//---------------------------------------------------------
Poly3D& Poly3D::operator=(const Poly3D& p)
//---------------------------------------------------------
{
  if (this == &p) { return (*this); }

  if ((p.m_N<1) || (p.m_Ntri<1)) 
  {
    this->Clear();          // p is empty?
  } 
  else 
  {
    m_N     = p.m_N;        // deep-copy data from p
    m_Ntri  = p.m_Ntri;
    m_xyz   = p.m_xyz;
    m_x     = p.m_x;
    m_y     = p.m_y;
    m_z     = p.m_z;
    m_areas = p.m_areas;
  }
  return (*this);
}


//---------------------------------------------------------
void Poly3D::Clear() 
//---------------------------------------------------------
{
  m_N = 0; m_Ntri = 0;
  m_xyz.destroy(); m_x.destroy(); m_y.destroy(); 
  m_z.destroy(); m_areas.destroy();
}


//---------------------------------------------------------
bool Poly3D::HavePoint(const DVec& point) 
//---------------------------------------------------------
{
  const double* p1 = point.data();

  double tol = 1e-6, normi=0.0; DVec pnti,tv;
  for (int i=1; i<=m_N; ++i) 
  {
    const double* p2 = m_xyz.pCol(i);
    normi = sqrt( SQ(p1[0]-p2[0]) + 
                  SQ(p1[1]-p2[1]) + 
                  SQ(p1[2]-p2[2]) );
    if (normi < tol) { return true; }
  }
  return false;
}


//---------------------------------------------------------
void Poly3D::AddPoint(const DVec& point) 
//---------------------------------------------------------
{
  if (!HavePoint(point)) {
    // append this point
    m_xyz.append_col(3, (double*)point.data());
    ++m_N;
  }
}


//---------------------------------------------------------
void Poly3D::AddTriangle(const DVec& v1, const DVec& v2, const DVec& v3)
//---------------------------------------------------------
{
  // Get the area of the triangle to be added
  double a = TriArea3D(v1, v2, v3);
  double tol = 1e-6;

  // Check to make sure that we have a nondegenerate triangle
  if (a > tol) 
  {
    DVec x_vals, y_vals, z_vals;
    x_vals.set(v1(1), v2(1), v3(1));
    y_vals.set(v1(2), v2(2), v3(2));
    z_vals.set(v1(3), v2(3), v3(3));

    m_x.append_col(3, x_vals.data());
    m_y.append_col(3, y_vals.data());
    m_z.append_col(3, z_vals.data());

    m_areas.append(a);
    ++m_Ntri;
  }
}


//---------------------------------------------------------
void Poly3D::BuildTriangles()
//---------------------------------------------------------
{
  // Calculate the center of the polygon

  umWARNING("Poly3D::BuildTriangles()", "TODO: Check from where this is called!");

  int Nr=m_xyz.num_rows(), Nc=m_xyz.num_cols();
  DMat t_xyz; t_xyz.borrow(Nr,Nc, m_xyz.data());
  DVec cent = t_xyz.row_sums()  / (double)m_N;
//DVec cent = sum(poly.m_xyz,2) / (double)m_N;

  // Sort the points by angle where the angle is 
  // measured from the center of the polygon
  SortPoints(cent);

  // now build triangles
  DVec a,b; double totalarea = 0.0;
  for (int i=1; i<=m_N; ++i) {
    a = t_xyz(All,i);
  //b = poly.m_xyz(All, mod(i,poly.m_N)+1);
    b = t_xyz(All, 1 + (i%m_N));
    AddTriangle(cent, a, b);
    totalarea += m_areas(i);
  }

  // Clear the polygon if the total area 
  // of the polygon is too small.
  double tol = 1e-6;
  if (totalarea < tol) {
    Clear();
  }
}


//---------------------------------------------------------
void Poly3D::SortPoints(const DVec& cent)
//---------------------------------------------------------
{
  // Sort the points by angle from cent which is
  // a point in the plane of the polygon.  This
  // is done in a counter-clockwise direction.

  // If less than 2 points, no need to sort
  if (m_N < 2) {
    return;
  }
  
  // create local cartesian axis
  DVec ref1,ref2,nor,axis1,axis2,vI,vJ,tmp;

  // wrap DM with DMat for utility routines
  int Nr=m_xyz.num_rows(), Nc=m_xyz.num_cols();
  DMat t_xyz; t_xyz.borrow(Nr, Nc, m_xyz.data());
  
  t_xyz.get_col(1) - cent;   ref1 /= ref1.norm(); 


  // get two lines in the plane
  ref1 = t_xyz.get_col(1) - cent;   ref1 /= ref1.norm(); 
  ref2 = t_xyz.get_col(2) - cent;   ref2 /= ref2.norm();

  // normal to the plane (norm = ref1 X ref2)
  nor(1) = ref1(2)*ref2(3) - ref1(3)*ref2(2);
  nor(2) = ref1(3)*ref2(1) - ref1(1)*ref2(3);
  nor(3) = ref1(1)*ref2(2) - ref1(2)*ref2(1);

  nor /= nor.norm();

  // axis definition
  axis1 = ref1;

  // axis2 = norm x axis1
  axis2(1) = nor(2)*axis1(3) - nor(3)*axis1(2);
  axis2(2) = nor(3)*axis1(1) - nor(1)*axis1(3);
  axis2(3) = nor(1)*axis1(2) - nor(2)*axis1(1);

  double costhetaI,sinthetaI, costhetaJ,sinthetaJ, thetaI,thetaJ;

  for (int i=1; i<=m_N; ++i) {
    for (int j=(i+1); j<=m_N; ++j) {
      vI = t_xyz.get_col(i) - cent;
      vJ = t_xyz.get_col(j) - cent;

      costhetaI = vI.inner(axis1);
      sinthetaI = vI.inner(axis2);

      costhetaJ = vJ.inner(axis1);
      sinthetaJ = vJ.inner(axis2);

      thetaI = atan2(sinthetaI, costhetaI);
      thetaJ = atan2(sinthetaJ, costhetaJ);
      
      // sort  minimum angle difference first
      if (thetaJ < thetaI) 
      {
        // swap I and J
      //t_xyz(All, [i j]) = t_xyz(All, [j i]);

        tmp = t_xyz.get_col(i);               // copy column i
        t_xyz.set_col(i, t_xyz.get_col(j));   // overwrite col i
        t_xyz.set_col(j, tmp);                // overwrite col j
      }
    }
  } 
}




///////////////////////////////////////////////////////////
//
// helper routines (used by class Poly3D)
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
bool PointInTri3D(const DM& TRI, const DVec& point)
//---------------------------------------------------------
{
  DVec va, vb, vc;

  DM& tri = const_cast<DM&>(TRI);
  va.borrow(3, tri.pCol(1)); vb.borrow(3, tri.pCol(2)); vc.borrow(3, tri.pCol(3));

  double area  = TriArea3D(va, vb, vc);
  double area1 = TriArea3D(va, vb, point);
  double area2 = TriArea3D(va, vc, point);
  double area3 = TriArea3D(vb, vc, point);

  double tol = 1e-6;
  double val = fabs(area1+area2+area3 - area);
  return (val < tol) ? true : false;
}


//---------------------------------------------------------
double TriArea3D(const DVec& Xa, const DVec& Xb, const DVec& Xc)
//---------------------------------------------------------
{
  double x1 = Xb(1)-Xa(1);
  double y1 = Xb(2)-Xa(2);
  double z1 = Xb(3)-Xa(3);
  
  double x2 = Xc(1)-Xa(1);
  double y2 = Xc(2)-Xa(2);
  double z2 = Xc(3)-Xa(3);
  
  double c1 = y1*z2-z1*y2;
  double c2 = z1*x2-x1*z2;
  double c3 = x1*y2-y1*x2;
  
  double area = 0.5*sqrt(c1*c1 + c2*c2 + c3*c3);
  return area;
}


//---------------------------------------------------------
bool EdgeIntersect3D
(
  const DVec& a1,   // [in]
  const DVec& b1,   // [in]
  const DVec& a2,   // [in]
  const DVec& b2,   // [in]
        DVec& xint  // [out]
)
//---------------------------------------------------------
{
  bool test = false;
  xint = -999.0;      // set xint to dummy vaklue

  double cx,cy,cz, dx,dy,dz, ex,ey,ez, t1,t2,mag,d;

  cx  = (b2(2)-a2(2))*(b1(3)-a1(3)) - (b2(3)-a2(3))*(b1(2)-a1(2));
  cy  = (b2(3)-a2(3))*(b1(1)-a1(1)) - (b2(1)-a2(1))*(b1(3)-a1(3));
  cz  = (b2(1)-a2(1))*(b1(2)-a1(2)) - (b2(2)-a2(2))*(b1(1)-a1(1));

  dx  = (b2(2)-a2(2))*(a1(3)-a2(3)) - (b2(3)-a2(3))*(a1(2)-a2(2));
  dy  = (b2(3)-a2(3))*(a1(1)-a2(1)) - (b2(1)-a2(1))*(a1(3)-a2(3));
  dz  = (b2(1)-a2(1))*(a1(2)-a2(2)) - (b2(2)-a2(2))*(a1(1)-a2(1));

  ex  = (b1(2)-a1(2))*(a2(3)-a1(3)) - (b1(3)-a1(3))*(a2(2)-a1(2));
  ey  = (b1(3)-a1(3))*(a2(1)-a1(1)) - (b1(1)-a1(1))*(a2(3)-a1(3));
  ez  = (b1(1)-a1(1))*(a2(2)-a1(2)) - (b1(2)-a1(2))*(a2(1)-a1(1));

  t1  = -(cx*dx + cy*dy + cz*dz);
  t2  =   cx*ex + cy*ey + cz*ez;
  mag =   cx*cx + cy*cy + cz*cz;
  
  double tol1 = 1e-6;
  if (mag < tol1) {
    // lines are colinear -- should be caught by "in" test
    return false;
  }

  t1/=mag; t2/=mag;

  DVec x1(3), x2(3);

  x1(1) = a1(1) + t1*(b1(1)-a1(1));
  x1(2) = a1(2) + t1*(b1(2)-a1(2));
  x1(3) = a1(3) + t1*(b1(3)-a1(3));

  x2(1) = a2(1) + t2*(b2(1)-a2(1));
  x2(2) = a2(2) + t2*(b2(2)-a2(2));
  x2(3) = a2(3) + t2*(b2(3)-a2(3));

  // make sure the segments really intersect.
  d = SQ(x1(1)-x2(1)) + SQ(x1(2)-x2(2)) + SQ(x1(3)-x2(3));

  double tol2 = 1e-6;
  if (d>tol2) {
    return false;
  }
  
  if ( (t1 >     -tol2 ) && 
       (t2 >     -tol2 ) && 
       (t1 < (1.0+tol2)) && 
       (t2 < (1.0+tol2)) )
  {
    xint = x1;    // we have intersection!
    return true;
  }
  return false;
}


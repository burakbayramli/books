// FindLocalCoords3D.cpp
// function [rOUT, sOUT, tOUT] = FindLocalCoords3D(k, xi, yi, zi)
// 2007/10/03
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG3D.h"


//---------------------------------------------------------
void NDG3D::FindLocalCoords3D
(
        int   k, 
  const DVec& xi, 
  const DVec& yi, 
  const DVec& zi,
        DVec& rOUT, 
        DVec& sOUT, 
        DVec& tOUT
)
//---------------------------------------------------------
{
  // Globals3D;

  int v1=EToV(k,1), v2=EToV(k,2), v3=EToV(k,3), v4=EToV(k,4);

  double v1x=VX(v1), v2x=VX(v2), v3x=VX(v3), v4x=VX(v4);
  double v1y=VY(v1), v2y=VY(v2), v3y=VY(v3), v4y=VY(v4);
  double v1z=VZ(v1), v2z=VZ(v2), v3z=VZ(v3), v4z=VZ(v4);

  // x = v1x*(-1-r-s-t)/2 + v2x*(1+r)/2 + v3x*(1+s)/2 + v4x*(1+t)/2
  // y = v1y*(-1-r-s-t)/2 + v2y*(1+r)/2 + v3y*(1+s)/2 + v4y*(1+t)/2
  // z = v1z*(-1-r-s-t)/2 + v2z*(1+r)/2 + v3z*(1+s)/2 + v4z*(1+t)/2
  //
  // (v2x-v1x)  (v3x-v1x)  (v4x-v1x) * (r) = 2*x + v1x - v2x - v3x - v4x
  // (v2y-v1y)  (v3y-v1y)  (v4y-v1y) * (s) = 2*y + v1y - v2y - v3y - v4y
  // (v2z-v1z)  (v3z-v1z)  (v4z-v1z) * (t) = 2*z + v1z - v2z - v3z - v4z

  DMat A(3,3);
  A(1,1) = v2x-v1x;  A(1,2) = v3x-v1x;   A(1,3) = v4x-v1x;
  A(2,1) = v2y-v1y;  A(2,2) = v3y-v1y;   A(2,3) = v4y-v1y;
  A(3,1) = v2z-v1z;  A(3,2) = v3z-v1z;   A(3,3) = v4z-v1z;

  int len = xi.length();
  DMat rhs(3,len, "rhs"), rst("rst");

  rhs.set_row(1, 2.0*xi + (v1x-v2x-v3x-v4x) );
  rhs.set_row(2, 2.0*yi + (v1y-v2y-v3y-v4y) );
  rhs.set_row(3, 2.0*zi + (v1z-v2z-v3z-v4z) );

#if (0)
  dumpDMat(A,   "A");
  dumpDMat(rhs, "rhs");
#endif

  rst = A|rhs;

#if (0)
  dumpDMat(rst, "rst");
#endif


  rOUT = rst.get_row(1);
  sOUT = rst.get_row(2);
  tOUT = rst.get_row(3);

#if (0)
  dumpDVec(rOUT, "rOUT");
  dumpDVec(sOUT, "sOUT");
  dumpDVec(tOUT, "tOUT");
#endif
}

// Curl2D.m
// function [vx,vy,vz] = Curl2D(ux,uy,uz);
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"

// NBN: comments
// define two overloaded versions:
//    a:  [--,--,vz] = C(ux,uy,--) 
//    b:  [vx,vy,vz] = C(ux,uy,uz) 


//---------------------------------------------------------
void NDG2D::Curl2D
(
  const DMat& ux,   // [in]
  const DMat& uy,   // [in]
        DMat& vz    // [out]
)
//---------------------------------------------------------
{
  // function [--,--,vz] = Curl2D(ux,uy,--);
  // Purpose: Compute 2D curl-operator in (x,y) plane

  DMat uxr = Dr*ux, uxs = Ds*ux, 
       uyr = Dr*uy, uys = Ds*uy;

  vz = rx.dm(uyr) + sx.dm(uys) - ry.dm(uxr) - sy.dm(uxs);
}


//---------------------------------------------------------
void NDG2D::Curl2D
(
  const DMat& ux,   // [in]
  const DMat& uy,   // [in]
  const DMat& uz,   // [in]
        DMat& vx,   // [out]
        DMat& vy,   // [out]
        DMat& vz    // [out]
)
//---------------------------------------------------------
{
  // function [vx,vy,vz] = Curl2D(ux,uy,uz);
  // Purpose: Compute 3D curl-operator

  DMat uxr = Dr*ux, uxs = Ds*ux, 
       uyr = Dr*uy, uys = Ds*uy,
       uzr = Dr*uz, uzs = Ds*uz;

  vz =  rx.dm(uyr) + sx.dm(uys) - ry.dm(uxr) - sy.dm(uxs);
  vx =  ry.dm(uzr) + sy.dm(uzs);
  vy = -rx.dm(uzr) - sx.dm(uzs);
}

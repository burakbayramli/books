// Grad3D.m
// function [dUdx, dUdy, dUdz] = GradH3D(U)
// 2007/06/11
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG3D.h"


//---------------------------------------------------------
void NDG3D::Grad3D
(
  const DMat& U,     // [in]
        DMat& dUdx,  // [out]
        DMat& dUdy,  // [out]
        DMat& dUdz   // [out]
)
//---------------------------------------------------------
{
  // function [dUdx, dUdy, dUdz] = GradH3D(U)
  // purpose: compute local elemental physical spatial derivatives of U    

  // compute local derivatives on reference tetrahedron  
  DMat dUdr = Dr*U,  dUds = Ds*U,  dUdt = Dt*U;

  // compute physical spatial derivatives using the chain rule
  dUdx = rx.dm(dUdr) + sx.dm(dUds) + tx.dm(dUdt);
  dUdy = ry.dm(dUdr) + sy.dm(dUds) + ty.dm(dUdt);
  dUdz = rz.dm(dUdr) + sz.dm(dUds) + tz.dm(dUdt);
}

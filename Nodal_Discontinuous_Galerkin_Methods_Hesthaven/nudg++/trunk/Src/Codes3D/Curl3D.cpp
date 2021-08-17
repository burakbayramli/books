// Curl3D.m
// function [curlx, curly, curlz] = CurlH3D(Ux, Uy, Uz)
// 2007/06/11
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG3D.h"

//---------------------------------------------------------
void NDG3D::Curl3D
(
  const DMat& Ux,     // [in]
  const DMat& Uy,     // [in]
  const DMat& Uz,     // [in]
        DMat& curlx,  // [out]
        DMat& curly,  // [out]
        DMat& curlz   // [out]
)
//---------------------------------------------------------
{
  // function [curlx, curly, curlz] = CurlH3D(Ux, Uy, Uz)
  // purpose: compute local elemental physical spatial curl of (Ux,Uy,Uz)

  // compute local derivatives of Ux on reference tetrahedron  
  DMat ddr = Dr*Ux,  dds = Ds*Ux,  ddt = Dt*Ux;

  // increment curl components
  curly =  (rz.dm(ddr) + sz.dm(dds) + tz.dm(ddt));
  curlz = -(ry.dm(ddr) + sy.dm(dds) + ty.dm(ddt));

  // compute local derivatives of Uy on reference tetrahedron  
  ddr = Dr*Uy;  dds = Ds*Uy;  ddt = Dt*Uy;

  // increment curl components
  curlx  =  -(rz.dm(ddr) + sz.dm(dds) + tz.dm(ddt));
  curlz +=   (rx.dm(ddr) + sx.dm(dds) + tx.dm(ddt));

  // compute local derivatives of Uz on reference tetrahedron  
  ddr = Dr*Uz;  dds = Ds*Uz;  ddt = Dt*Uz;

  // increment curl components
  curlx +=  (ry.dm(ddr) + sy.dm(dds) + ty.dm(ddt));
  curly -=  (rx.dm(ddr) + sx.dm(dds) + tx.dm(ddt));
}

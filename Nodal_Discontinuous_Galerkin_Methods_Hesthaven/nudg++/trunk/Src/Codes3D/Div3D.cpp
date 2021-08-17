// Div3D.m
// function [divU] = DivH3D(Ux, Uy, Uz)
// 2007/06/11
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG3D.h"

//---------------------------------------------------------
void NDG3D::Div3D
(
  const DMat& Ux, 
  const DMat& Uy, 
  const DMat& Uz, 
        DMat& divU
)
//---------------------------------------------------------
{
  // function [divU] = DivH3D(Ux, Uy, Uz)
  // purpose: compute local elemental physical spatial divergence of (Ux,Uy,Uz)

  // compute local derivatives of Ux on reference tetrahedron  
  DMat ddr = Dr*Ux,  dds = Ds*Ux,  ddt = Dt*Ux;

  // dUx/dx
  divU =  (rx.dm(ddr) + sx.dm(dds) + tx.dm(ddt));

  // compute local derivatives of Uy on reference tetrahedron  
  ddr = Dr*Uy;  dds = Ds*Uy;  ddt = Dt*Uy;

  // add dUy/dy to divergence
  divU =  divU + (ry.dm(ddr) + sy.dm(dds) + ty.dm(ddt));

  // compute local derivatives of Uz on reference tetrahedron  
  ddr = Dr*Uz;  dds = Ds*Uz;  ddt = Dt*Uz;

  // add dUz/dz to divergence
  divU =  divU + (rz.dm(ddr) + sz.dm(dds) + tz.dm(ddt));
}

// PhysDmatrices3D.cpp
// function [Dx,Dy,Dz] = PhysDmatrices3D(x1, y1, z1, interp)
// 2007/10/07
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG3D.h"


//---------------------------------------------------------
void NDG3D::PhysDmatrices3D
(
  const DVec& x1,     // [in]
  const DVec& y1,     // [in]
  const DVec& z1,     // [in]
  const DMat& interp, // [in]
        DMat& Dx,     // [out]
        DMat& Dy,     // [out]
        DMat& Dz      // [out]
)
//---------------------------------------------------------
{
  // function [Dx,Dy,Dz] = PhysDmatrices3D(x1, y1, z1, interp)
  // Purpose : Initialize the (x,y,z) differentiation matrices
  //	    on the simplex, evaluated at (x1,y1,z1) at order N

  DMat_Diag  RX, SX, TX,  RY, SY, TY,  RZ, SZ, TZ;
  DVec       rx1,sx1,tx1, ry1,sy1,ty1, rz1,sz1,tz1, J1;

  DMat IDr=interp*Dr, IDs=interp*Ds, IDt=interp*Dt;

  ::GeometricFactors3D(x1,y1,z1,IDr,IDs,IDt, // [in]
                       rx1,sx1,tx1,          // [out]
                       ry1,sy1,ty1,          // [out]
                       rz1,sz1,tz1, J1);     // [out]

  // load vectors as diagonal matrices
  RX = rx1, SX = sx1, TX = tx1;
  RY = ry1, SY = sy1, TY = ty1;
  RZ = rz1, SZ = sz1, TZ = tz1;

  Dx = RX*IDr + SX*IDs + TX*IDt; 
  Dy = RY*IDr + SY*IDs + TY*IDt; 
  Dz = RZ*IDr + SZ*IDs + TZ*IDt; 
}

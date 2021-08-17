// PhysDmatrices2D.m
// function [Dx,Dy] = PhysDmatrices2D(x1, y1, interp)
// 2007/10/07
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"


//---------------------------------------------------------
void NDG2D::PhysDmatrices2D
(
  const DVec& x1,     // [in]
  const DVec& y1,     // [in]
  const DMat& interp, // [in]
        DMat& Dx,     // [out]
        DMat& Dy      // [out]
)
//---------------------------------------------------------
{
  // function [Dr,Ds] = PhysDmatrices2D(x1, y1, interp)
  // Purpose : Initialize the (x,y) differentiation matrices
  //	    on the simplex, evaluated at (x1,y1) at order N

  DMat_Diag RX, SX, RY, SY;
  DVec      rx1,sx1,ry1,sy1, J1;

  DMat IDr=interp*Dr, IDs=interp*Ds;

  ::GeometricFactors2D(x1, y1, IDr, IDs,      // [in]
                       rx1,sx1,ry1,sy1,J1);   // [out]

  // load vectors as diagonal matrices
  RX = rx1;  SX = sx1;
  RY = ry1;  SY = sy1;

  Dx = RX*IDr + SX*IDs;
  Dy = RY*IDr + SY*IDs;
}

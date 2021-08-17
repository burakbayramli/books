// InterpNodeShapes3D.cpp
// function interpmat = InterpNodeShapes3D(k, xi, yi, zi)
// 2007/10/03
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG3D.h"


//---------------------------------------------------------
DMat& NDG3D::InterpNodeShapes3D
(
        int   k,
  const DVec& xi,
  const DVec& yi,
  const DVec& zi
)
//---------------------------------------------------------
{
  DVec rOUT, sOUT, tOUT;
  FindLocalCoords3D(k, xi, yi, zi, rOUT, sOUT, tOUT);

  DMat *tmp = new DMat("(outV/V)", OBJ_temp);
  DMat& interpmat=(*tmp);

  DMat outV = Vandermonde3D(N, rOUT, sOUT, tOUT);
  interpmat = outV/this->V;
  return interpmat;
}

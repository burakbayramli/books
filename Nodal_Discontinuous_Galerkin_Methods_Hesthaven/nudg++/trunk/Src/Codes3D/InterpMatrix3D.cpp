// InterpMatrix3D.cpp
// function [IM] = InterpMatrix3D(rout, sout, tout)
// 2007/09/23
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG3D.h"


//---------------------------------------------------------
DMat& NDG3D::InterpMatrix3D(const DVec& rout, const DVec& sout, const DVec& tout)
//---------------------------------------------------------
{
  // function [IM] = InterpMatrix3D(rout, sout, tout)
  // purpose: compute local elemental interpolation matrix

  DMat *IM = new DMat("IM", OBJ_temp), Vout;
   
  // compute Vandermonde at (rout,sout,tout)
  Vout = Vandermonde3D(N, rout, sout, tout);

  // build interpolation matrix
  (*IM) = Vout * this->invV;
  return (*IM);
}


//---------------------------------------------------------
void NDG3D::InterpMatrix3D(Cub3D& cub)
//---------------------------------------------------------
{
  // compute Vandermonde at (rout,sout)
  DMat Vout = Vandermonde3D(this->N, cub.R, cub.S, cub.T);

  // build interpolation matrix
  cub.V = Vout * this->invV;

  // store transpose
  cub.VT = trans(cub.V);
}

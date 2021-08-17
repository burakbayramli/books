// InterpMatrix2D.m
// function [IM] = InterpMatrix2D(rout, sout)
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"


//---------------------------------------------------------
DMat& NDG2D::InterpMatrix2D(const DVec& rout, const DVec& sout)
//---------------------------------------------------------
{
  // function [IM] = InterpMatrix2D(rout, sout)
  // purpose: compute local elemental interpolation matrix

  DMat *IM = new DMat("IM", OBJ_temp), Vout;
   
  // compute Vandermonde at (rout,sout)
  Vout = Vandermonde2D(N, rout, sout);

  // build interpolation matrix
  (*IM) = Vout * this->invV;
  return (*IM);
}


//---------------------------------------------------------
void NDG2D::InterpMatrix2D(Cub2D& cub)
//---------------------------------------------------------
{
  // compute Vandermonde at (rout,sout)
  DMat Vout = Vandermonde2D(this->N, cub.r, cub.s);

  // build interpolation matrix
  cub.V = Vout * this->invV;

  // store transpose
  cub.VT = trans(cub.V);
}

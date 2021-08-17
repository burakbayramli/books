// Vandermonde1D.m
// function [V1D] = Vandermonde1D(N,xp)
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"

//---------------------------------------------------------
DMat& Vandermonde1D(int N, const DVec& xp)
//---------------------------------------------------------
{
  // function [V1D] = Vandermonde1D(N,xp)
  // Purpose : Initialize the 1D Vandermonde Matrix.
  //	    V_{ij} = phi_j(xp_i);

  DMat *p1D = new DMat(xp.size(),N+1, 0.0, OBJ_temp, "V1D");
  DMat& V1D = (*p1D);  // shorthand

  for (int j=1; j<=(N+1); ++j) {
    V1D(All,j) = JacobiP(xp, 0, 0, j-1);
  }
  return V1D;
}

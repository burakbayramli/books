// Vandermonde2D.cpp
// function [V2D] = Vandermonde2D(N, r, s);
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"


//---------------------------------------------------------
DMat& Vandermonde2D(int N, const DVec& r, const DVec& s)
//---------------------------------------------------------
{
  // function [V2D] = Vandermonde2D(N, r, s);
  // Purpose : Initialize the 2D Vandermonde Matrix.
  //           V_{ij} = phi_j(r_i, s_i);

  DMat *p2D = new DMat(r.size(),(N+1)*(N+2)/2, 0.0, OBJ_temp, "V2D");
  DMat& V2D = (*p2D);  // shorthand

  // Transfer to (a,b) coordinates
  DVec a,b;  rstoab(r,s, a,b);

  // build the Vandermonde matrix
  int sk = 1;
  for (int i=0; i<=N; ++i) {
    for (int j=0; j<=(N-i); ++j) {
      V2D(All,sk) = Simplex2DP(a,b,i,j);
      ++sk;
    }
  }

  return V2D;
}

// Vandermonde2D.cpp
// function [V3D] = Vandermonde3D(N, r, s, t);
// 2007/06/10
//---------------------------------------------------------
#include "NDGLib_headers.h"


//---------------------------------------------------------
DMat& Vandermonde3D(int N, const DVec& r, const DVec& s, const DVec& t)
//---------------------------------------------------------
{
  // function [V3D] = Vandermonde3D(N, r, s, t);
  // Purpose : Initialize the 3D Vandermonde Matrix.
  //           V_{ij} = phi_j(r_i, s_i, t_i);

  DMat *p3D = new DMat(r.size(),(N+1)*(N+2)*(N+3)/6, 0.0, OBJ_temp, "V3D");
  DMat& V3D = (*p3D);  // shorthand

  // Transfer to (a,b,c) coordinates
  DVec a,b,c;  rsttoabc(r,s,t, a,b,c);

  // build the Vandermonde matrix
  int sk = 1;
  for (int i=0; i<=N; ++i) { // old ordering
    for (int j=0; j<=(N-i); ++j) {
      for (int k=0; k<=(N-i-j); ++k) {
        V3D(All,sk) = Simplex3DP(a,b,c, i,j,k);
        ++sk;
      }
    }
  }
  return V3D;
}

// Simplex3DP.m
// function [P] = Simplex3DP(a,b,c,i,j,k);
// 2007/06/11
//---------------------------------------------------------
#include "NDGLib_headers.h"


//---------------------------------------------------------
DVec& Simplex3DP(const DVec& a, const DVec& b, const DVec& c, int i, int j, int k)
//---------------------------------------------------------
{
  // function [P] = Simplex3DP(a,b,c,i,j,k);
  // Purpose : Evaluate 3D orthonormal polynomial
  //           on simplex at (a,b,c) of order (i,j,k).

  DVec* P = new DVec("P", OBJ_temp);

  DVec h1 = JacobiP(a,0.0,0.0,i), h2 = JacobiP(b,(2.0*i+1),0.0,j), h3 = JacobiP(c,(2.0*(i+j)+2),0.0,k);

//P  = 2.0*sqrt(2.0)*h1 .* h2 .* ((1.0-b).^i).*h3 .* ((1.0-c).^(i+j));
  
  DVec tv1 = 2.0*sqrt(2.0)*h1.dm(h2);
  DVec tv2 = pow(1.0-b,(double)i);
  DVec tv3 = h3.dm(pow(1.0-c,(double)(i+j)));

  (*P) = tv1.dm( tv2.dm(tv3) );
  return (*P);
}

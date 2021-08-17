// Simplex2DP.m
// function [P] = Simplex2DP(a,b,i,j);
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"


//---------------------------------------------------------
DVec& Simplex2DP(const DVec& a, const DVec& b, int i, int j)
//---------------------------------------------------------
{
  // function [P] = Simplex2DP(a,b,i,j);
  // Purpose : Evaluate 2D orthonormal polynomial
  //           on simplex at (a,b) of order (i,j).

  DVec* P = new DVec("P", OBJ_temp);
  DVec h1 = JacobiP(a,0.0,0.0,i), h2 = JacobiP(b,2.0*i+1,0.0,j);
  DVec tv1=sqrt(2.0)*h1.dm(h2), tv2=pow(1.0-b,(double)i);
  (*P) = tv1.dm(tv2);
  return (*P);
}

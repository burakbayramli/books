// JacobiGL.m
// function [x] = JacobiGL(alpha,beta,N);
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"

//---------------------------------------------------------
DVec& JacobiGL(double alpha, double beta, int N)
//---------------------------------------------------------
{
  // function [x] = JacobiGL(alpha,beta,N)
  // Purpose: Compute the N'th order Gauss Lobatto quadrature 
  //          points, x, associated with the Jacobi polynomial,
  //          of type (alpha,beta) > -1 ( <> -0.5). 

  DVec* x = new DVec(N+1, 0.0, OBJ_temp);
  if (N==1) { (*x)(1)=-1.0; (*x)(2)=1.0; return (*x); }

  DVec xint(1), w(1); Index1D II(2,N);
  JacobiGQ(alpha+1,beta+1,N-2,  xint,w, true);

  // assemble result: sandwich eigenvalues between [-1,1]
  (*x)(1)=-1.0; (*x)(II)=xint; (*x)(N+1)=1.0;
  return (*x);
}

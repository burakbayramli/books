// Dmatrices2D.m
// function [Dr,Ds] = Dmatrices2D(N,r,s,V)
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"


// global version
//---------------------------------------------------------
void Dmatrices2D
(
        int   N,  // [in]
  const DVec& r,  // [in]
  const DVec& s,  // [in]
  const DMat& V,  // [in]
        DMat& Dr, // [out]
        DMat& Ds  // [out]
)
//---------------------------------------------------------
{
  // function [Dr,Ds] = Dmatrices2D(N,r,s,V)
  // Purpose : Initialize the (r,s) differentiation matrices
  //	    on the simplex, evaluated at (r,s) at order N

  DMat Vr,Vs; GradVandermonde2D(N, r, s, Vr, Vs);
  Dr = Vr/V; Ds = Vs/V;
}


// member version
//---------------------------------------------------------
void NDG2D::Dmatrices2D()
//---------------------------------------------------------
{
  DMat Vr,Vs; GradVandermonde2D(N, r, s, Vr, Vs);
  this->Dr = Vr/this->V;
  this->Ds = Vs/this->V;
}


// member version (cubature)
//---------------------------------------------------------
void NDG2D::Dmatrices2D(int Nc, Cub2D& cub)
//---------------------------------------------------------
{
  DMat Vr,Vs; GradVandermonde2D(Nc, cub.r, cub.s, Vr, Vs);

  cub.Dr  = Vr/this->V;  // cub.dphidr
  cub.Ds  = Vs/this->V;  // cub.dphids

  // store transposes
  cub.DrT = trans(cub.Dr);
  cub.DsT = trans(cub.Ds);
}

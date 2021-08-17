// Dmatrices3D.m
// function [Dr,Ds,Dt] = Dmatrices3D(N,r,s,t,V)
// 2007/09/23
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG3D.h"


// global version
//---------------------------------------------------------
void Dmatrices3D
(
        int   N,  // [in]
  const DVec& r,  // [in]
  const DVec& s,  // [in]
  const DVec& t,  // [in]
  const DMat& V,  // [in]
        DMat& Dr, // [out]
        DMat& Ds, // [out]
        DMat& Dt  // [out]
)
//---------------------------------------------------------
{
  // function [Dr,Ds,Dt] = Dmatrices3D(N,r,s,t,V)
  // Purpose : Initialize the (r,s,t) differentiation matrices
  //	    on the simplex, evaluated at (r,s,t) at order N

  DMat Vr,Vs,Vt; GradVandermonde3D(N, r,s,t, Vr,Vs,Vt);
  Dr = Vr/V; Ds = Vs/V; Dt = Vt/V;
}


// member version
//---------------------------------------------------------
void NDG3D::Dmatrices3D()
//---------------------------------------------------------
{
  DMat Vr,Vs,Vt; GradVandermonde3D(N, r,s,t, Vr,Vs,Vt);
  this->Dr = Vr/this->V;
  this->Ds = Vs/this->V;
  this->Dt = Vt/this->V;
}


// member version (cubature)
//---------------------------------------------------------
void NDG3D::Dmatrices3D(int Nc, Cub3D& cub)
//---------------------------------------------------------
{
  DMat Vr,Vs,Vt; 
  GradVandermonde3D(Nc, cub.R,cub.S,cub.T, Vr,Vs,Vt);

  cub.Dr  = Vr/this->V;  // cub.dphidr
  cub.Ds  = Vs/this->V;  // cub.dphids
  cub.Dt  = Vt/this->V;  // cub.dphids

  // store transposes
  cub.DrT = trans(cub.Dr);
  cub.DsT = trans(cub.Ds);
  cub.DtT = trans(cub.Dt);
}

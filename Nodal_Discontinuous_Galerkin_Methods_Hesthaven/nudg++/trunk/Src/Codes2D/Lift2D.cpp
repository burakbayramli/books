// Lift2D.m
// function [LIFT] = Lift2D()
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"


//---------------------------------------------------------
DMat& NDG2D::Lift2D()
//---------------------------------------------------------
{
  // function [LIFT] = Lift2D()
  // Purpose  : Compute surface to volume lift term for DG formulation

  DMat V1D,massEdge1,massEdge2,massEdge3;  DVec faceR,faceS;
  Index1D J1(1,Nfp), J2(Nfp+1,2*Nfp), J3(2*Nfp+1,3*Nfp);

  DMat Emat(Np, Nfaces*Nfp);

  // face 1
  faceR = r(Fmask(All,1));
  V1D = Vandermonde1D(N, faceR); 
  massEdge1 = inv(V1D*trans(V1D));
  Emat(Fmask(All,1), J1) = massEdge1;

  // face 2
  faceR = r(Fmask(All,2));
  V1D = Vandermonde1D(N, faceR);
  massEdge2 = inv(V1D*trans(V1D));
  Emat(Fmask(All,2), J2) = massEdge2;

  // face 3
  faceS = s(Fmask(All,3));
  V1D = Vandermonde1D(N, faceS);
  massEdge3 = inv(V1D*trans(V1D));
  Emat(Fmask(All,3), J3) = massEdge3;

  // inv(mass matrix)*\I_n (L_i,L_j)_{edge_n}
  LIFT = V*(trans(V)*Emat);
  return LIFT;
}

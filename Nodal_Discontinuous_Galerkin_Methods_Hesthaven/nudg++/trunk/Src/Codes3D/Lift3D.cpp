// Lift3D.m
// function [LIFT] = Lift3D(N,R,S,T)
// 2007/06/11
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG3D.h"


//---------------------------------------------------------
DMat& NDG3D::Lift3D()
//---------------------------------------------------------
{
  // function [LIFT] = Lift3D(N, r, s, t)
  // Purpose  : Compute 3D surface to volume lift operator used in DG formulation

  DMat Emat(Np, Nfaces*Nfp),VFace,massFace; 
  DVec faceR,faceS; IVec idr;  Index1D JJ;

  for (int face=1; face<=Nfaces; ++face) {
    // process face
    if      (1==face) {faceR = r(Fmask(All,1)); faceS = s(Fmask(All,1));}
    else if (2==face) {faceR = r(Fmask(All,2)); faceS = t(Fmask(All,2));}
    else if (3==face) {faceR = s(Fmask(All,3)); faceS = t(Fmask(All,3));}
    else if (4==face) {faceR = s(Fmask(All,4)); faceS = t(Fmask(All,4));}
    
    VFace = Vandermonde2D(N, faceR, faceS);
    massFace = inv(VFace*trans(VFace));

    idr = Fmask(All,face);
  //idc    = (face-1)*Nfp+1: face*Nfp;
    JJ.reset((face-1)*Nfp+1, face*Nfp);

  //Emat(idr, JJ) = Emat(idr, JJ) + massFace;
    Emat(idr, JJ) = massFace;  // TW: += -> =
  }

  // inv(mass matrix)*\I_n (L_i,L_j)_{edge_n}
  LIFT = V*(trans(V)*Emat);
  return LIFT;
}

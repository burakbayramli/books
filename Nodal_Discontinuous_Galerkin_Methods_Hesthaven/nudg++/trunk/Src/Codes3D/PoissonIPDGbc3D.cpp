// PoissonIPDGbc3D.cpp
// function [bc] = PoissonIPDGbc3D(ubc)
// 2007/09/22
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG3D.h"


//---------------------------------------------------------
DMat& NDG3D::PoissonIPDGbc3D(DVec& ubc)
//---------------------------------------------------------
{
  // function [bc] = PoissonIPDGbc3D(ubc)
  //
  // Purpose: Set up the discrete Poisson matrix directly
  //          using LDG. The operator is set up in the weak form


  DVec faceR("faceR"), faceS("faceS"), faceT("faceT");
  DMat V2D("V2D"), Dx("Dx"),Dy("Dy"),Dz("Dz"), Dn1("Dn1"), mmE_Fm1("mmE(:,Fm1)");
  IVec Fm("Fm"), Fm1("Fm1"), fidM("fidM");
  double lnx=0.0,lny=0.0,lnz=0.0,lsJ=0.0,hinv=0.0,gtau=0.0;
  int i=0,k1=0,f1=0,id=0;
  IVec i1_Nfp = Range(1,Nfp);
  double N1N1 = double((N+1)*(N+1));

  // build local face matrices
  DMat massEdge[5]; // = zeros(Np,Np,Nfaces);
  for (i=1; i<=Nfaces; ++i) {
    massEdge[i].resize(Np,Np);
  }

  // face mass matrix 1
  Fm = Fmask(All,1); faceR = r(Fm); faceS = s(Fm); 
  V2D = Vandermonde2D(N, faceR, faceS);
  massEdge[1](Fm,Fm) = inv(V2D*trans(V2D));

  // face mass matrix 2
  Fm = Fmask(All,2); faceR = r(Fm); faceT = t(Fm);
  V2D = Vandermonde2D(N, faceR, faceT);
  massEdge[2](Fm,Fm) = inv(V2D*trans(V2D));

  // face mass matrix 3
  Fm = Fmask(All,3); faceS = s(Fm); faceT = t(Fm);
  V2D = Vandermonde2D(N, faceS, faceT); 
  massEdge[3](Fm,Fm) = inv(V2D*trans(V2D));

  // face mass matrix 4
  Fm = Fmask(All,4); faceS = s(Fm); faceT = t(Fm);
  V2D = Vandermonde2D(N, faceS, faceT); 
  massEdge[4](Fm,Fm) = inv(V2D*trans(V2D));

  // build DG right hand side
  DMat* pBC = new DMat(Np, K, "bc", OBJ_temp); 
  DMat& bc = (*pBC);  // reference, for syntax

  for (k1=1; k1<=K; ++k1)
  {
    if (! (k1%100)) { umMSG(1, "%d, ",k1); }

    for (f1=1; f1<=Nfaces; ++f1) {

      if (EToE(k1,f1)==k1) 
      {
        Fm1 = Fmask(All,f1); 
        fidM  = (k1-1)*Nfp*Nfaces + (f1-1)*Nfp + i1_Nfp;
        id = 1+(f1-1)*Nfp + (k1-1)*Nfp*Nfaces;

        lnx = nx(id);  lny = ny(id);  lnz = nz(id); 
        lsJ = sJ(id); hinv = Fscale(id);

        Dx = rx(1,k1)*Dr + sx(1,k1)*Ds + tx(1,k1)*Dt;  
        Dy = ry(1,k1)*Dr + sy(1,k1)*Ds + ty(1,k1)*Dt;
        Dz = rz(1,k1)*Dr + sz(1,k1)*Ds + tz(1,k1)*Dt;
        Dn1 = lnx*Dx + lny*Dy +lnz*Dz;

      //mmE = lsJ*massEdge(:,:,f1);
      //bc(All,k1) += (gtau*mmE(All,Fm1) - Dn1'*mmE(All,Fm1))*ubc(fidM);

        mmE_Fm1 = massEdge[f1](All,Fm1);  mmE_Fm1 *= lsJ;

        gtau = 2.0*N1N1*hinv; // set penalty scaling
        bc(All,k1) += (gtau*mmE_Fm1 - trans(Dn1)*mmE_Fm1) * ubc(fidM);
      }
    }
  }
  umMSG(1, "\n");

  return bc; 
}



// OpStructs2D.h
// 
// 2007/08/27
//---------------------------------------------------------
#ifndef NDG__OpStructs2D_H__INCLUDED
#define NDG__OpStructs2D_H__INCLUDED

#include "CS_Type.h"

// forward
// class DGops2D
// class BCops2D


//---------------------------------------------------------
class DGops2D
//---------------------------------------------------------
{
  // information for N'th order elements 
public:

  DGops2D() : Nfp(0), K(0) {}
  ~DGops2D() { }
  
  // DGops2D& operator=(const DGops2D& I) 
  // { this->K = I.K; umERROR("DGops2D& operator=()", "TODO"); return (*this); }

  // resize member arrays
  void resize(int bc_Nfp, int bc_K) 
  {
    this->Nfp = bc_Nfp; 
    this->K = bc_K;

    map.resize(Nfp, K); 
    elmts.resize(K); 
    faces.resize(K);

    Diff.resize(Nfp*Nfp, K); // zeros(Nfp, Nfp, bc.K);
    LIFT.resize(Nfp*  2, K); // zeros(Nfp,   2, bc.K);

    x.resize(Nfp, K);
    y.resize(Nfp, K);
    H.resize(K);
    signedH.resize(K);
    normal.resize(2,K);
    mapB.resize(2);
  }

public:

  // 1D dg systems for each edge
  int Nfp, K;
  DMat x,y, fx,fy, normal, Diff, LIFT;
  DMat genDiff, genLIFT, genInvJac, genSignedInvJac;
  DVec H, signedH;

  IMat map, vmap, vvmapM, vvmapP;
  IVec elmts, faces, mapB, vmapB;
};


//---------------------------------------------------------
class BCops2D
//---------------------------------------------------------
{
  // information for N'th order elements 
public:

  BCops2D() : q(0) {}
  ~BCops2D() {}

  int q;
  
  // Boundary matrices {E,N,W,S} and LU factors
  CSd   eA,eB,eC, nA,nB,nC, wA,wB,wC, sA,sB,sC;
  CS_LU eLU,      nLU,      wLU,      sLU;

  // Boundary matrices {NE,NW,SW,SE} and LU factors
  CSd   neA,neC,  nwA,nwC,  swA,swC,  seA,seC;
  CS_LU neLU,     nwLU,     swLU,     seLU;

  DVec nG, eG, wG, sG;
};

#endif  // NDG__OpStructs2D_H__INCLUDED

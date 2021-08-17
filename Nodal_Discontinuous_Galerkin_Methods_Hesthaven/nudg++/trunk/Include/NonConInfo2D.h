// NonConInfo2D.h
// 
// 2007/08/18
//---------------------------------------------------------
#ifndef NDG__NonConInfo2D_H__INCLUDED
#define NDG__NonConInfo2D_H__INCLUDED


// forward
class PInfo;
class FInfo;

#include <vector>
typedef std::vector<FInfo*> FInfoV;   // face info for H non-con
typedef std::vector<PInfo*> PInfoV;   // mode info for P non-con


//---------------------------------------------------------
class PInfo
//---------------------------------------------------------
{
  // information for N'th order elements 
public:

  PInfo(int maxN) 
    : Nmax(maxN), Np(0), Nfp(0), K(0)
  {
    // allow 1-based indexing of the following 
    // arrays, by skipping element[0] in each 

    interpP = new DMat[Nmax+1]; assert(interpP);
    fmapM   = new IVec[Nmax+1]; assert(fmapM);
    vmapP   = new IMat[Nmax+1]; assert(vmapP);
  }

  ~PInfo() {
    if (interpP) { delete [] interpP; interpP=NULL; }
    if (fmapM)   { delete [] fmapM;   fmapM=NULL; }
    if (vmapP)   { delete [] vmapP;   vmapP=NULL; }
  }
  
  PInfo& operator=(const PInfo& B) { 
    Np=B.Np; Nfp=B.Nfp; K=B.K;
    Fmask=B.Fmask; ids=B.ids; ks=B.ks; mapW=B.mapW;
    r=B.r; s=B.s;  Dr=B.Dr; Ds=B.Ds; LIFT=B.LIFT;  V=B.V;
    rx=B.rx; ry=B.ry; sx=B.sx; sy=B.sy; J=B.J; sJ=B.sJ;
    x=B.x; y=B.y; nx=B.nx; ny=B.ny; Fscale=B.Fscale; 
    // TODO: copy interpP, fmapM, vmapP, ...
    return (*this); 
  }

public:
  int   Nmax,Np,Nfp,K;
  IMat  Fmask,ids,tri;
  IVec  ks,mapW;
  DVec  r,s;
  DMat  Dr,Ds,LIFT,V;
  DMat  rx,ry,sx,sy,J,sJ;
  DMat  x,y,nx,ny,Fscale;

  DMat* interpP;    // interp matrices for all necessary orders.
  IVec* fmapM;      // used as 1D vector of ids, each possible order
  IMat* vmapP;      // used as 2D matrix of ids, each possible order
};


//---------------------------------------------------------
class FInfo
//---------------------------------------------------------
{
  // information for non-conforming faces
public:

  FInfo() : elmtM(0),elmtP(0),faceM(0),faceP(0),nx(0.0),ny(0.0) {}
  ~FInfo() {}
  
  FInfo& operator=(const FInfo& B) { 
    elmtM=B.elmtM; elmtP=B.elmtP; faceM=B.faceM; faceP=B.faceP;
    nx=B.nx; ny=B.ny; gVM=B.gVM; gVP=B.gVP; lift=B.lift;
    return (*this); 
  }

public:
  int elmtM,elmtP, faceM,faceP;
  double nx,ny;
  DMat gVM, gVP, lift;
};

#endif  // NDG__NonConInfo2D_H__INCLUDED

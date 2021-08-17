// Cub2D.h
// 2D Cubature data
// 2007/06/06
//---------------------------------------------------------
#ifndef NDG__Cub2D_H__INCLUDED
#define NDG__Cub2D_H__INCLUDED

#include "Mat_COL.h"

//---------------------------------------------------------
class Cub2D
//---------------------------------------------------------
{
public:
  Cub2D();
  int Ncub;
  DVec r, s, w;
  DMat W;
  DMat V, Dr, Ds,  VT, DrT, DsT;
  DMat x,y, rx, sx, ry, sy, J;
  DMat mm, mmCHOL;
};


//---------------------------------------------------------
class Gauss2D
//---------------------------------------------------------
{
public:
  Gauss2D();
  void resize(int Ng, int K, int Nfaces);
  int NGauss;
  DVec z, w;
  DMat x, y, W;
  IMat mapM, mapP;
  IVec mapI,mapO,mapW,mapC,mapS,mapD,mapN,mapB;
  DMat nx, ny, J, sJ, rx, ry, sx, sy;
  DMat interp, interpT, finterp[4];
};


//---------------------------------------------------------
class CInfo2D
//---------------------------------------------------------
{
public:
  CInfo2D() : elmt(0) {}

  void resize(int Ng, int Nf){
    // these need to be sized before use
    gnx.resize(Ng,Nf); gx.resize(Ng,Nf);
    gny.resize(Ng,Nf); gy.resize(Ng,Nf);
  }
  
  int elmt;             // element number in global mesh
  DMat MM;              // mass matrix
  DMat Dx, Dy;          // physical derivative matrices
  DMat gnx,gx,gny,gy;   // normals and coordinates at Gauss nodes
  DMat gVM[4], gVP[4];  // Vandermondes for '-' and '+' traces
  DMat glift[4];        // matrices to lift Gauss node data
};


#endif  // NDG__Cub2D_H__INCLUDED

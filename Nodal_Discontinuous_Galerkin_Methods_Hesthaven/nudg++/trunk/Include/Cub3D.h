// Cub3D.h
// 3D Cubature data
// 2007/09/23
//---------------------------------------------------------
#ifndef NDG__Cub333D_H__INCLUDED
#define NDG__Cub333D_H__INCLUDED

#include "Mat_COL.h"

//---------------------------------------------------------
class Cub3D
//---------------------------------------------------------
{
public:
  Cub3D();
  int Ncub;
  DVec R, S, T, w;
  DMat W;
  DMat V, Dr, Ds, Dt,  VT, DrT, DsT, DtT;
  DMat x,y,z, rx,sx,tx, ry,sy,ty, rz,sz,tz, J;
  DMat mm, mmCHOL;
};


// FIXME: adjust for 3D
//---------------------------------------------------------
class Gauss3D
//---------------------------------------------------------
{
public:
  Gauss3D();
  void resize(int Ng, int K, int Nfaces);
  int NGauss;
  // DVec z, w;
  // DMat x, y, z, W;
  IMat mapM, mapP;
  IVec mapI,mapO,mapW,mapC,mapS,mapD,mapN,mapB;
  DMat nx,ny,nz, J, sJ, rx,ry,rz, sx,sy,sz, tx,ty,tz;
  DMat interp, interpT, finterp[5];
};



// FIXME: adjust for 3D
//---------------------------------------------------------
class CInfo3D
//---------------------------------------------------------
{
public:
  CInfo3D() : elmt(0) {}

  void resize(int Ng, int Nf){
    // these need to be sized before use
    gnx.resize(Ng,Nf); gx.resize(Ng,Nf);
    gny.resize(Ng,Nf); gy.resize(Ng,Nf);
  }
  
  int elmt;                   // element number in global mesh
  DMat MM;                    // mass matrix
  DMat Dx, Dy, Dz;            // physical derivative matrices
  DMat gnx,gx,gny,gy,gnz,gz;  // normals and coordinates at Gauss nodes
  DMat gVM[5], gVP[5];        // Vandermondes for '-' and '+' traces
  DMat glift[5];              // matrices to lift Gauss node data
};


#endif  // NDG__Cub333D_H__INCLUDED

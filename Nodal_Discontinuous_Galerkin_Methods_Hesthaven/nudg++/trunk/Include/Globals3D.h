// Globals3D.h
// containter for (3D) global data
// 2007/09/23
//---------------------------------------------------------
#ifndef NDG__Globals_333D_H__INCLUDED
#define NDG__Globals_333D_H__INCLUDED

#include "Global_funcs.h"
#include "Cub3D.h"
#include "VecObj_Type.h"


//---------------------------------------------------------
class Globals3D
//---------------------------------------------------------
{
public:

  Globals3D();
  virtual ~Globals3D();

  void init();
  void reset();
  void clear();

  int     Np, Nfp, N, K, Nfaces;
  double  NODETOL;
  DVec    r, s, t;
  DMat    Dr, Ds, Dt, LIFT, Drw, Dsw, Dtw, MassMatrix;
  DMat    Fx, Fy, Fz, nx, ny, nz, Fscale, sJ;


  IVec    vmapB, mapB;
  IMat    BCType, Fmask;
  IVec     mapI,  mapO,  mapW,  mapF,  mapC,  mapS,  mapM,  mapP,  mapD,  mapN;
  IVec    vmapI, vmapO, vmapW, vmapF, vmapC, vmapS, vmapM, vmapP, vmapD, vmapN;


  DMat    rx, ry, rz, sx, sy, sz, tx, ty, tz, J;
  DVec    rk4a, rk4b, rk4c;

  IMat    EToE, EToF, EToV, saveBCType;

  DMat    V, invV, VVT; // +NBN: precalculate V*V'
  DVec    VX, VY, VZ;   // +NBN: triangles on a sphere
  DMat    x, y, z;      // +NBN: triangles on a sphere

  //-------------------------------------
  // cubature and quadrature data for curved elements
  //-------------------------------------

  IVec straight, curved;    // map {straight,curved} bdry faces

  Cub3D   m_cub;            // 3D cubature data
//Gauss3D m_gauss;          // 3D surface data

  int     CubatureOrder;    // order for 3D cubature 
  int     NGauss;           // number of Gauss points

  VecObj<CInfo3D> m_cinfo;  // 3D curved face data


  //-------------------------------------
  // +NBN: added
  //-------------------------------------
  DVec    materialVals, epsilon;
  int     tstep, Nsteps;
  double  dt, time, FinalTime, RKtime, pi, eps;
  double  Xmin, Xmax, Ymin, Ymax, Zmin, Zmax;
  int     Nv, Nmats, Nbcs, Nsd;
  bool    bIs3D, bCoord3D, bElement3D;

};

#endif // NDG__Globals_333D_H__INCLUDED

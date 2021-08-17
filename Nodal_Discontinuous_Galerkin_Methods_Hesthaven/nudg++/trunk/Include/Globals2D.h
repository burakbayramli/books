// Globals2D.h
// containter for (2D) global data
// 2007/06/11
//---------------------------------------------------------
#ifndef NDG__Globals_22D_H__INCLUDED
#define NDG__Globals_22D_H__INCLUDED

#include "Global_funcs.h"
#include "Cub2D.h"
#include "VecObj_Type.h"



//---------------------------------------------------------
class Globals2D
//---------------------------------------------------------
{
public:

  Globals2D();
  virtual ~Globals2D();

  void init();
  void reset();
  void clear();

  int     Np, Nfp, N, K, Nfaces;
  double  NODETOL;
  DVec    r, s;
  DMat    Dr, Ds, LIFT, Drw, Dsw, MassMatrix;
  DMat    Fx, Fy, nx, ny, jac, Fscale;
  IVec    vmapB, mapB;
  IMat    BCType, Fmask;
  IVec     mapI,  mapO,  mapW,  mapF,  mapC,  mapS,  mapM,  mapP,  mapD,  mapN;
  IVec    vmapI, vmapO, vmapW, vmapF, vmapC, vmapS, vmapM, vmapP, vmapD, vmapN;
  // NBN: 2nd vmapO -> vmapF, add {vmapM, vmapP}


  DMat    rx, ry, sx, sy, J, sJ;
  DVec    rk4a, rk4b, rk4c;
  IMat    EToE, EToF, EToV, saveBCType;
  // NBN: add {saveBCType}

  DMat    V, invV, VVT; // +NBN: precalculate V*V'
  DVec    VX, VY, VZ;   // +NBN: triangles on a sphere
  DMat    x, y, z;      // +NBN: triangles on a sphere

  //-------------------------------------
  // cubature and quadrature data for curved elements
  //-------------------------------------

  IVec straight, curved;    // map {straight,curved} bdry faces

  Cub2D   m_cub;            // 2D cubature data
  Gauss2D m_gauss;          // 2D surface data

  int     CubatureOrder;    // order for 2D cubature 
  int     NGauss;           // number of Gauss points

  VecObj<CInfo2D> m_cinfo;  // 2D curved face data


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

#endif // NDG__Globals_22D_H__INCLUDED

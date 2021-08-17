// StartUp3D.m
// 
// 2007/06/14
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG3D.h"


//---------------------------------------------------------
bool NDG3D::StartUp3D()
//---------------------------------------------------------
{
  // Purpose : Setup script, building operators, grid, metric,
  //           and connectivity tables for 3D meshes of tetrahedra.

  // Definition of constants
  Np = (N+1)*(N+2)*(N+3)/6; Nfp = (N+1)*(N+2)/2; Nfaces=4; NODETOL = 1e-7;

  // Compute nodal set
  DVec x1,y1,z1;
  Nodes3D(N, x1,y1,z1);
  xyztorst(x1,y1,z1, r,s,t);

  // Build reference element matrices
  V = Vandermonde3D(N,r,s,t); invV = inv(V);
  MassMatrix = trans(invV)*invV;
  ::Dmatrices3D(N, r, s, t, V, Dr, Ds, Dt);


  // build coordinates of all the nodes
  IVec va = EToV(All,1), vb = EToV(All,2), vc = EToV(All,3), vd = EToV(All,4);
  x = 0.5*(-(1.0+r+s+t)*VX(va) + (1.0+r)*VX(vb) + (1.0+s)*VX(vc) + (1.0+t)*VX(vd));
  y = 0.5*(-(1.0+r+s+t)*VY(va) + (1.0+r)*VY(vb) + (1.0+s)*VY(vc) + (1.0+t)*VY(vd));
  z = 0.5*(-(1.0+r+s+t)*VZ(va) + (1.0+r)*VZ(vb) + (1.0+s)*VZ(vc) + (1.0+t)*VZ(vd));

  // find all the nodes that lie on each edge
  IVec fmask1,fmask2,fmask3,fmask4;
  fmask1 = find( abs(1.0+t),     '<', NODETOL); 
  fmask2 = find( abs(1.0+s),     '<', NODETOL);
  fmask3 = find( abs(1.0+r+s+t), '<', NODETOL);
  fmask4 = find( abs(1.0+r),     '<', NODETOL);
  Fmask.resize(Nfp,4);                          // set shape (M,N) before concat()
  Fmask = concat(fmask1,fmask2,fmask3,fmask4);  // load vector into shaped matrix

  Fx=x(Fmask,All); Fy=y(Fmask,All); Fz=z(Fmask,All);

  // Create surface integral terms
  Lift3D();

  // calculate geometric factors and normals
  Normals3D();
  
  Fscale = sJ.dd(J(Fmask,All));

  // Build connectivity matrix
  tiConnect3D(EToV, EToE, EToF); 

  // Build connectivity maps
  BuildMaps3D();

  // Compute weak operators (could be done in preprocessing to save time)
  DMat Vr,Vs,Vt;  GradVandermonde3D(N, r, s, t, Vr, Vs, Vt);

  VVT = V*trans(V);
  Drw = (V*trans(Vr))/VVT; Dsw = (V*trans(Vs))/VVT; Dtw = (V*trans(Vt))/VVT;

  return true;
}

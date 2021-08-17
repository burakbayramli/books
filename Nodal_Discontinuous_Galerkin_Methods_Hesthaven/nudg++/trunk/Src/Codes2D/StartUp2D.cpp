// StartUp2D.m
// 
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"


//---------------------------------------------------------
bool NDG2D::StartUp2D()
//---------------------------------------------------------
{
  // Purpose : Setup script, building operators, grid, metric,
  //           and connectivity tables.

  // Definition of constants
  Nfp = N+1; Np = (N+1)*(N+2)/2; Nfaces=3; NODETOL = 1e-12;

  // Compute nodal set
  DVec x1,y1; Nodes2D(N, x1,y1);  xytors(x1,y1, r,s);

  // Build reference element matrices
  V = Vandermonde2D(N,r,s); invV = inv(V);
  MassMatrix = trans(invV)*invV;
  ::Dmatrices2D(N,r,s,V, Dr,Ds);

  // build coordinates of all the nodes
  IVec va = EToV(All,1), vb = EToV(All,2), vc = EToV(All,3);

  // Note: outer products of (Vector,MappedRegion1D)
  x = 0.5 * (-(r+s)*VX(va) + (1.0+r)*VX(vb) + (1.0+s)*VX(vc));
  y = 0.5 * (-(r+s)*VY(va) + (1.0+r)*VY(vb) + (1.0+s)*VY(vc));

  // find all the nodes that lie on each edge
  IVec fmask1,fmask2,fmask3;
  fmask1 = find( abs(s+1.0), '<', NODETOL); 
  fmask2 = find( abs(r+s  ), '<', NODETOL);
  fmask3 = find( abs(r+1.0), '<', NODETOL);
  Fmask.resize(Nfp,3);                    // set shape (M,N) before concat()
  Fmask = concat(fmask1,fmask2,fmask3);   // load vector into shaped matrix

  Fx = x(Fmask, All); Fy = y(Fmask, All);

  // Create surface integral terms
  Lift2D();

  // calculate geometric factors
  ::GeometricFactors2D(x,y,Dr,Ds,  rx,sx,ry,sy,J);

  // calculate geometric factors
  Normals2D();
  Fscale = sJ.dd(J(Fmask,All));


#if (0)
  OutputNodes(false); // volume nodes
  OutputNodes(true);  // face nodes
  umERROR("Exiting early", "Check {volume,face} nodes");
#endif

  // Build connectivity matrix
  tiConnect2D(EToV, EToE,EToF);

  // Build connectivity maps
  BuildMaps2D();

  // Compute weak operators (could be done in preprocessing to save time)
  DMat Vr,Vs;  GradVandermonde2D(N, r, s, Vr, Vs);
  VVT = V*trans(V);
  Drw = (V*trans(Vr))/VVT;  Dsw = (V*trans(Vs))/VVT;

  return true;
}

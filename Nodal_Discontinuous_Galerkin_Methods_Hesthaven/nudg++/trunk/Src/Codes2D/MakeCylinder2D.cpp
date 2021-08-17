// MakeCylinder2D.m
// function MakeCylinder2D(faces, ra,xo,yo)
// 2007/10/16
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"


//---------------------------------------------------------
void NDG2D::MakeCylinder2D
(
  const IMat& faces, 
  double ra,
  double xo,
  double yo
)
//---------------------------------------------------------
{
  // Function: MakeCylinder2D(faces, ra, xo, yo)
  // Purpose:  Use Gordon-Hall blending with an isoparametric  
  //           map to modify a list of faces so they conform 
  //           to a cylinder of radius r centered at (xo,yo)

  int NCurveFaces = faces.num_rows();
  IVec vflag(VX.size()); IMat VFlag(EToV.num_rows(),EToV.num_cols());
  int n=0,k=0,f=0,v1=0,v2=0;
  double theta1=0.0,theta2=0.0,x1=0.0,x2=0.0,y1=0.0,y2=0.0;
  double newx1=0.0,newx2=0.0,newy1=0.0,newy2=0.0;
  DVec vr, fr, theta, fdx,fdy, vdx, vdy, blend,numer,denom;
  IVec va,vb,vc, ks, Fm_f, ids;  DMat Vface, Vvol;

  for (n=1; n<=NCurveFaces; ++n)
  {
    // move vertices of faces to be curved onto circle
    k = faces(n,1); f = faces(n,2);
    v1 = EToV(k, f); v2 = EToV(k, umMOD(f,Nfaces)+1);
    theta1 = atan2(VY(v1),VX(v1)); theta2 = atan2(VY(v2),VX(v2));
    newx1 = xo + ra*cos(theta1); newy1 = yo + ra*sin(theta1);
    newx2 = xo + ra*cos(theta2); newy2 = yo + ra*sin(theta2);
    
    // update mesh vertex locations
    VX(v1) = newx1; VX(v2) = newx2; VY(v1) = newy1; VY(v2) = newy2; 

    // store modified vertex numbers
    vflag(v1) = 1;  vflag(v2) = 1;
  }

  // map modified vertex flag to each element
//vflag = vflag(EToV);
  VFlag.set_map(EToV, vflag); // (map, values)

  // locate elements with at least one modified vertex
//ks = find(sum(vflag,2)>0);
  ks = find(VFlag.row_sums(), '>', 0);

  // build coordinates of all the corrected nodes
  IMat VA=EToV(ks,1), VB=EToV(ks,2), VC=EToV(ks,3);

  // FIXME: loading 2D mapped data into 1D vectors
  int Nr=VA.num_rows(); 
  va.copy(Nr,VA.data());
  vb.copy(Nr,VB.data());
  vc.copy(Nr,VC.data());

  // Note: outer products of (Vector,MappedRegion1D)
  x(All,ks) = 0.5*(-(r+s)*VX(va)+(1.0+r)*VX(vb)+(1.0+s)*VX(vc));
  y(All,ks) = 0.5*(-(r+s)*VY(va)+(1.0+r)*VY(vb)+(1.0+s)*VY(vc));

  // deform specified faces
  for (n=1; n<=NCurveFaces; ++n)
  {
    k = faces(n,1); f = faces(n,2);

    // find vertex locations for this face and tangential coordinate
    if      (f==1) { v1=EToV(k,1); v2=EToV(k,2); vr=r; }
    else if (f==2) { v1=EToV(k,2); v2=EToV(k,3); vr=s; }
    else if (f==3) { v1=EToV(k,1); v2=EToV(k,3); vr=s; }

    fr = vr(Fmask(All,f));
    x1 = VX(v1); y1 = VY(v1); x2 = VX(v2); y2 = VY(v2);

    // move vertices at end points of this face to the cylinder
    theta1 = atan2(y1-yo, x1-xo); theta2 = atan2(y2-yo, x2-xo);

    // check to make sure they are in the same quadrant
    if ((theta2 > 0.0) && (theta1 < 0.0)) { theta1 += 2*pi; }
    if ((theta1 > 0.0) && (theta2 < 0.0)) { theta2 += 2*pi; }

    // distribute N+1 nodes by arc-length along edge
    theta = 0.5*theta1*(1.0-fr) + 0.5*theta2*(1.0+fr);

    // evaluate deformation of coordinates
    fdx = xo + ra*apply(cos,theta) - x(Fmask(All,f),k); 
    fdy = yo + ra*apply(sin,theta) - y(Fmask(All,f),k);

    // build 1D Vandermonde matrix for face nodes and volume nodes
    Vface = Vandermonde1D(N, fr); Vvol = Vandermonde1D(N, vr);

    // compute unblended volume deformations 
    vdx = Vvol * (Vface|fdx); vdy = Vvol * (Vface|fdy);

    // blend deformation and increment node coordinates
    ids = find(abs(1.0-vr), '>', 1e-7); // warp and blend

    denom = 1.0-vr(ids);
    if      (1==f) { numer = -(r(ids)+s(ids));  }
    else if (2==f) { numer =  (r(ids)+1.0   );  }
    else if (3==f) { numer = -(r(ids)+s(ids));  }
    blend = numer.dd(denom);

    //-----------------------------------
    // x(ids,k) += blend.dm(vdx(ids));    // VC++ compiler specific?
    // y(ids,k) += blend.dm(vdy(ids));
    //-----------------------------------
    // unroll the above operator
    int Nr = ids.size(), id=0;
    for (int m=1; m<=Nr; ++m) { 
      id=ids(m); 
      x(id,k) += blend(m)*vdx(id);
      y(id,k) += blend(m)*vdy(id);
    }
  }

  // repair other coordinate dependent information
  Fx = x(Fmask, All); Fy = y(Fmask, All);
  ::GeometricFactors2D(x,y,Dr,Ds,  rx,sx,ry,sy,J);
  Normals2D(); Fscale = sJ.dd(J(Fmask,All));
}

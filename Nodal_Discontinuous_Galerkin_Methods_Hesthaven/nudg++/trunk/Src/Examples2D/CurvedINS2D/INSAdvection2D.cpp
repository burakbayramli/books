// INSAdvection2D.m
// compute the advection terms for incompressible Navier-Stokes
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedINS2D.h"

//---------------------------------------------------------
void CurvedINS2D::INSAdvection2D()
//---------------------------------------------------------
{
  // reuse 4 static allocations (set size before use!)
  static DMat UxM, UxP, UyM, UyP;
  UxM.resize(Nfp*Nfaces, K); UxP.resize(Nfp*Nfaces, K); 
  UyM.resize(Nfp*Nfaces, K); UyP.resize(Nfp*Nfaces, K); 

  DMat fxUx,fyUx,fxUy,fyUy, fxUxM,fyUxM,fxUyM,fyUyM, fxUxP,fyUxP,fxUyP,fyUyP;
  DMat UDotNM,UDotNP,maxvel, fluxUx,fluxUy;
  double t1 = timer.read();

  // evaluate flux vectors
  fxUx=sqr(Ux);  fyUx=Ux.dm(Uy);  fxUy=Ux.dm(Uy); fyUy=sqr(Uy);

  // save old nonlinear terms
  NUxold = NUx; NUyold = NUy; 

  // evaluate inner-product of test function gradient and flux functions
  Div2D(fxUx,fyUx, NUx);  Div2D(fxUy,fyUy, NUy);

  // interpolate velocity to face nodes on element faces
  UxM = Ux(vmapM); UyM = Uy(vmapM);
  UxP = Ux(vmapP); UyP = Uy(vmapP);

  // set '+' trace of velocity at boundary face nodes
  UxP(mapI) = bcUx(mapI);   UyP(mapI) = bcUy(mapI);
  UxP(mapW) = bcUx(mapW);   UyP(mapW) = bcUy(mapW);
  UxP(mapC) = bcUx(mapC);   UyP(mapC) = bcUy(mapC);

  // evaluate flux vectors at '-' and '+' traces at face nodes
  fxUxM=sqr(UxM);  fyUxM=UxM.dm(UyM);  fxUyM=UxM.dm(UyM); fyUyM=sqr(UyM);
  fxUxP=sqr(UxP);  fyUxP=UxP.dm(UyP);  fxUyP=UxP.dm(UyP); fyUyP=sqr(UyP);

  // evaluate dot product of normal and velocity at face nodes
  UDotNM = UxM.dm(nx) + UyM.dm(ny);  UDotNP = UxP.dm(nx) + UyP.dm(ny);
  maxvel = max(abs(UDotNM), abs(UDotNP));

  // evaluate maximum normal velocity at face face nodes
  maxvel.reshape(Nfp, Nfaces*K);
  maxvel = outer(ones(Nfp), maxvel.max_col_vals());
  maxvel.reshape(Nfp*Nfaces, K);

  // form local Lax-Friedrichs/Rusonov fluxes
  fluxUx = 0.5*( -nx.dm(fxUxM-fxUxP) - ny.dm(fyUxM-fyUxP) - maxvel.dm(UxP-UxM) );
  fluxUy = 0.5*( -nx.dm(fxUyM-fxUyP) - ny.dm(fyUyM-fyUyP) - maxvel.dm(UyP-UyM) );

  // put volume and surface terms together
  NUx += LIFT*(Fscale.dm(fluxUx));
  NUy += LIFT*(Fscale.dm(fluxUy));

  // compute (U~,V~)
  UxT = ((a0*Ux + a1*Uxold) - dt*(b0*NUx + b1*NUxold))/g0; 
  UyT = ((a0*Uy + a1*Uyold) - dt*(b0*NUy + b1*NUyold))/g0; 

  //---------------------------
  time_advection += timer.read() - t1;
  //---------------------------
}

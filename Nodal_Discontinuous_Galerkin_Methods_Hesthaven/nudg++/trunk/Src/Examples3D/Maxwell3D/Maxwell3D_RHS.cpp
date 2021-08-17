// Maxwell2D_RHS.m
// function [rhsHx, rhsHy, rhsHz, rhsEx, rhsEy, rhsEz] = ...
//           MaxwellRHS3D(Hx,Hy,Hz,Ex,Ey,Ez)
// 2007/06/11
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "Maxwell3D.h"

//---------------------------------------------------------
void Maxwell3D::RHS()
//---------------------------------------------------------
{
  // function [rhsHx, rhsHy, rhsHz, rhsEx, rhsEy, rhsEz] = ...
  //                          MaxwellRHS3D(Hx,Hy,Hz,Ex,Ey,Ez)
  // Purpose  : Evaluate RHS flux in 3D Maxwell equations

  //---------------------------
  double t1 = timer.read();
  //---------------------------

  // form field differences at faces
  dHx = Hx(vmapP)-Hx(vmapM);  dEx = Ex(vmapP)-Ex(vmapM);	
  dHy = Hy(vmapP)-Hy(vmapM); 	dEy = Ey(vmapP)-Ey(vmapM);	
  dHz = Hz(vmapP)-Hz(vmapM);  dEz = Ez(vmapP)-Ez(vmapM);  

  // make boundary conditions all reflective (Ez+ = -Ez-)
  dHx(mapB) = 0.0;  dEx(mapB) = -2.0*Ex(vmapB); 
  dHy(mapB) = 0.0;  dEy(mapB) = -2.0*Ey(vmapB); 
  dHz(mapB) = 0.0;  dEz(mapB) = -2.0*Ez(vmapB);

  alpha=1.0; // => full upwinding

  ndotdH = nx.dm(dHx) + ny.dm(dHy) + nz.dm(dHz);
  ndotdE = nx.dm(dEx) + ny.dm(dEy) + nz.dm(dEz);

  fluxHx = -ny.dm(dEz) + nz.dm(dEy) + alpha*(dHx - ndotdH.dm(nx)); 
  fluxHy = -nz.dm(dEx) + nx.dm(dEz) + alpha*(dHy - ndotdH.dm(ny)); 
  fluxHz = -nx.dm(dEy) + ny.dm(dEx) + alpha*(dHz - ndotdH.dm(nz)); 

  fluxEx =  ny.dm(dHz) - nz.dm(dHy) + alpha*(dEx - ndotdE.dm(nx)); 
  fluxEy =  nz.dm(dHx) - nx.dm(dHz) + alpha*(dEy - ndotdE.dm(ny)); 
  fluxEz =  nx.dm(dHy) - ny.dm(dHx) + alpha*(dEz - ndotdE.dm(nz)); 

  // evaluate local spatial derivatives
  Curl3D(Hx,Hy,Hz,  curlHx,curlHy,curlHz);
  Curl3D(Ex,Ey,Ez,  curlEx,curlEy,curlEz);

  // calculate Maxwell's right hand side
  rhsHx = -curlEx + LIFT*(Fscale.dm(fluxHx)/2.0);
  rhsHy = -curlEy + LIFT*(Fscale.dm(fluxHy)/2.0);
  rhsHz = -curlEz + LIFT*(Fscale.dm(fluxHz)/2.0);

  rhsEx =  curlHx + LIFT*(Fscale.dm(fluxEx)/2.0);
  rhsEy =  curlHy + LIFT*(Fscale.dm(fluxEy)/2.0);
  rhsEz =  curlHz + LIFT*(Fscale.dm(fluxEz)/2.0);

  //---------------------------
  time_rhs += timer.read() - t1;
  //---------------------------
}

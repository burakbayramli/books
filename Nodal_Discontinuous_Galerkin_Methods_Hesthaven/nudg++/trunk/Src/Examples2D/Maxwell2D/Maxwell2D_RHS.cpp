// Maxwell2D_RHS.m
// function [rhsHx, rhsHy, rhsEz] = MaxwellRHS2D(Hx,Hy,Ez)
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "Maxwell2D.h"

//---------------------------------------------------------
void Maxwell2D::RHS()
//---------------------------------------------------------
{
  // function [rhsHx, rhsHy, rhsEz] = MaxwellRHS2D(Hx,Hy,Ez)
  // Purpose  : Evaluate RHS flux in 2D Maxwell TM form 

  //---------------------------
  double t1 = timer.read();
  //---------------------------

  // Define field differences at faces
  dHx = Hx(vmapM)-Hx(vmapP);
  dHy = Hy(vmapM)-Hy(vmapP); 
  dEz = Ez(vmapM)-Ez(vmapP);

  // Impose reflective boundary conditions (Ez+ = -Ez-)
  dHx(mapB)=0.0; dHy(mapB)=0.0; dEz(mapB)=2.0*Ez(vmapB);

  // evaluate upwind fluxes
  alpha = 1.0; 
  ndotdH =  nx.dm(dHx) + ny.dm(dHy);
  fluxHx =  ny.dm(dEz) + alpha*(ndotdH.dm(nx) - dHx);
  fluxHy = -nx.dm(dEz) + alpha*(ndotdH.dm(ny) - dHy);
  fluxEz = -nx.dm(dHy) + ny.dm(dHx) - alpha*dEz;

  // local derivatives of fields
  Grad2D(Ez, Ezx,Ezy);  Curl2D(Hx,Hy, CuHz);

  // compute right hand sides of the PDE's
  rhsHx = -Ezy  + LIFT*(Fscale.dm(fluxHx))/2.0;
  rhsHy =  Ezx  + LIFT*(Fscale.dm(fluxHy))/2.0;
  rhsEz =  CuHz + LIFT*(Fscale.dm(fluxEz))/2.0;

  //---------------------------
  time_rhs += timer.read() - t1;
  //---------------------------
}

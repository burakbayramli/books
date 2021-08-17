// MaxwellNonCon2D_RHS.cpp
// function [rhsHx, rhsHy, rhsEz] = MaxwellRHS2D(Hx,Hy,Ez)
// 2007/08/04
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "MaxwellNonCon2D.h"


//---------------------------------------------------------
void MaxwellNonCon2D::RHS()
//---------------------------------------------------------
{
  if (eModeH == noncon_mode) {
    RHS_H();
  } else {
    RHS_P();
  }
}


//---------------------------------------------------------
void MaxwellNonCon2D::RHS_base()
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


//---------------------------------------------------------
void MaxwellNonCon2D::RHS_H()
//---------------------------------------------------------
{
  // function [rhsHx, rhsHy, rhsEz] = MaxwellHNonConRHS2D(Hx,Hy,Ez, neighbors)
  // Purpose  : Evaluate RHS flux in 2D Maxwell TM form 

  // 1.0 Only apply PEC boundary conditions at wall boundaries 
  savemapB = mapB; savevmapB = vmapB; mapB = mapW;  vmapB = vmapW;

  // 1.1 Evaluate right hand side 
  // [rhsHx, rhsHy, rhsEz] = MaxwellRHS2D(Hx, Hy, Ez);
  this->RHS_base();

  // 1.2 Restore original boundary node lists
  mapB = savemapB; vmapB = savevmapB;

  int k1=0,k2=0; double lnx=0.0,lny=0.0;

  DVec ldHx,ldHy,ldEz, lndotdH,fluxHx,fluxHy,fluxEz;

  //---------------------------
  double t1 = timer.read();
  //---------------------------

  // 2.0 Correct lifted fluxes at each non-conforming face fragment
  for (int n=1; n<=Nnoncon; ++n) 
  {
    const FInfo& neigh = (*m_FInfo[n]);

    // 2.1 Extract information about this non-conforming face fragment
    k1=neigh.elmtM; k2=neigh.elmtP; lnx=neigh.nx; lny=neigh.ny;        
    const DMat &gVM=neigh.gVM, &gVP=neigh.gVP;
    
    // 2.2 Compute difference of traces at Gauss nodes on face fragment
    ldHx = gVM*Hx(All,k1) - gVP*Hx(All,k2);
    ldHy = gVM*Hy(All,k1) - gVP*Hy(All,k2);
    ldEz = gVM*Ez(All,k1) - gVP*Ez(All,k2);
    
    // 2.3 Compute flux terms at Gauss nodes on face fragment
    lndotdH =  lnx*ldHx + lny*ldHy;
    fluxHx  =  lny*ldEz + lnx*lndotdH - ldHx;
    fluxHy  = -lnx*ldEz + lny*lndotdH - ldHy;
    fluxEz  = -lnx*ldHy + lny*ldHx    - ldEz;
    
    // 2.4 Lift fluxes for non-conforming face fragments and update residuals
    const DMat& lift = neigh.lift;
    rhsHx(All,k1) += 0.5*(lift*fluxHx);
    rhsHy(All,k1) += 0.5*(lift*fluxHy);
    rhsEz(All,k1) += 0.5*(lift*fluxEz);
  }

  //---------------------------
  time_rhs_H += timer.read() - t1;
  //---------------------------
}


//---------------------------------------------------------
void MaxwellNonCon2D::RHS_P()
//---------------------------------------------------------
{
  //---------------------------
  double t3 = timer.read();
  //---------------------------

  // function [rhsHx, rhsHy, rhsEz] = MaxwellPNonConRHS2D(pinfo, Hx,Hy,Ez)
  // Purpose  : Evaluate RHS flux in 2D Maxwell TM form 

  // For each possible polynomial order
  for (N=1; N<=Nmax; ++N)
  {
    // Extract information for this polynomial order
    const PInfo& pinf = *(m_PInfo[N]);

    K = pinf.K;

    // Check to see if any elements of this order exist
    if (K>0) {

      // Find location of N'th order nodes
      const IMat &ids=pinf.ids, &Fmask=pinf.Fmask;  int Nr=0,Nc=0;
      DMat dR, dHxdr,dHydr,dEzdr, dHxds,dHyds,dEzds, dHxdy,dHydx,dEzdx,dEzdy;


      // Extract N'th order nodes
      DMat HxN=Hx.get_map(ids), HyN=Hy.get_map(ids), EzN=Ez.get_map(ids);

      // Extract '-' traces of N'th order nodal data
      DMat HxM=HxN(Fmask,All), HyM=HyN(Fmask,All), EzM=EzN(Fmask,All);
      
      // Storage for '+' traces
      HxM.size(Nr,Nc);  DMat HxP(Nr,Nc), HyP(Nr,Nc), EzP(Nr,Nc);

      // For each possible order
      for (int N2=1; N2<=Nmax; ++N2) {

        // Check to see if any neighbor nodes of this order were located
        if ( pinf.fmapM[N2].length() > 0) {

          // L2 project N2'th order neighbor data onto N'th order trace space
          const DMat& interp = pinf.interpP[N2];
          const IVec& fmapM  = pinf.fmapM[N2];
          const IMat& vmapP  = pinf.vmapP[N2];

          HxP(fmapM) = interp*Hx.get_map(vmapP);
          HyP(fmapM) = interp*Hy.get_map(vmapP);
          EzP(fmapM) = interp*Ez.get_map(vmapP);
        }
      }

      // Compute jumps of trace data at faces
      dHx = HxM-HxP;  dHy = HyM-HyP;  dEz = EzM-EzP;

      if (pinf.mapW.size() > 0) {
        // Apply PEC boundary condition at wall boundary faces
        dHx(pinf.mapW) = 0.0;
        dHy(pinf.mapW) = 0.0;
        dEz(pinf.mapW) = 2.0*EzM(pinf.mapW);
      }

      // evaluate jump in incoming characteristic variable
      dR = -pinf.ny.dm(dHx) + pinf.nx.dm(dHy) + dEz;

      // Compute flux terms
      fluxHx =  pinf.ny.dm(dR);
      fluxHy = -pinf.nx.dm(dR);
      fluxEz =            -dR;

      // Evaluate local derivatives of fields
      dHxdr = pinf.Dr*HxN; dHxds = pinf.Ds*HxN;
      dHydr = pinf.Dr*HyN; dHyds = pinf.Ds*HyN;
      dEzdr = pinf.Dr*EzN; dEzds = pinf.Ds*EzN;

      // Compute physical derivatives of fields
      dHxdy = pinf.ry.dm(dHxdr) + pinf.sy.dm(dHxds);
      dHydx = pinf.rx.dm(dHydr) + pinf.sx.dm(dHyds);
      dEzdx = pinf.rx.dm(dEzdr) + pinf.sx.dm(dEzds);
      dEzdy = pinf.ry.dm(dEzdr) + pinf.sy.dm(dEzds);

      // Compute right hand sides of the PDE's
      rhsHx(ids) = -dEzdy         + pinf.LIFT*(pinf.Fscale.dm(fluxHx))/2.0;
      rhsHy(ids) =  dEzdx         + pinf.LIFT*(pinf.Fscale.dm(fluxHy))/2.0;
      rhsEz(ids) =  dHydx - dHxdy + pinf.LIFT*(pinf.Fscale.dm(fluxEz))/2.0;
    }
  }

  //---------------------------
  time_rhs_P += timer.read() - t3;
  //---------------------------
}

// INSLiftDrag2D.m
// function [Cd, Cl, dP, sw, stri] = 
//  INSLiftDrag2D(Ux, Uy, PR, ra, nu, time, tstep, Nsteps)
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedINS2D.h"

//---------------------------------------------------------
void CurvedINS2D::INSLiftDrag2D(double ra)
//---------------------------------------------------------
{
  // function [Cd, Cl, dP, sw, stri] = INSLiftDrag2D(Ux, Uy, PR, ra, nu, time, tstep, Nsteps)
  // Purpose: compute coefficients of lift, drag and pressure drop at cylinder

  static FILE* fid; 
  static DVec sw1, sw2; 
  static int Nc=0, stri1=0, stri2=0;

  if (1 == tstep) {
    char buf[50]; sprintf(buf, "liftdraghistory%d.dat", N);
    fid = fopen(buf, "w");
    fprintf(fid, "timeCdCldP = [...\n");

    // Sample location and weights for pressure drop
    // Note: the VolkerCylinder test assumes the 2 
    // sample points (-ra, 0), (ra, 0), are vertices 
    // located on the internal cylinder boundary

    Sample2D(-ra, 0.0,  sw1, stri1);
    Sample2D( ra, 0.0,  sw2, stri2);

    Nc = mapC.size()/Nfp;
  }

  bool bDo=false; 
  if (1==tstep || tstep==Nsteps || !umMOD(tstep,10)) { bDo=true; }
  else if (time > 3.90 && time < 3.96) { bDo=true; } // catch C_d  (3.9362)
  else if (time > 5.65 && time < 5.73) { bDo=true; } // catch C_l  (5.6925)
  else if (time > 7.999) { bDo=true; } // catch dP  (8.0)
  if (!bDo) return;

  DVec PRC, nxC, nyC, wv, tv;
  DMat dUxdx,dUxdy, dUydx,dUydy, MM1D, V1D;
  DMat dUxdxC,dUxdyC, dUydxC,dUydyC, hforce, vforce, sJC;
  double dP=0.0, Cd=0.0, Cl=0.0;

  dP = sw1.inner(PR(All,stri1)) - sw2.inner(PR(All,stri2));

  // compute derivatives
  Grad2D(Ux, dUxdx,dUxdy);  dUxdxC=dUxdx(vmapC); dUxdyC=dUxdy(vmapC);
  Grad2D(Uy, dUydx,dUydy);  dUydxC=dUydx(vmapC); dUydyC=dUydy(vmapC);

  PRC=PR(vmapC); nxC=nx(mapC); nyC=ny(mapC); sJC=sJ(mapC);
  hforce = -PRC.dm(nxC) + nu*( nxC.dm(  2.0 *dUxdxC) + nyC.dm(dUydxC+dUxdyC) );
  vforce = -PRC.dm(nyC) + nu*( nxC.dm(dUydxC+dUxdyC) + nyC.dm(  2.0 *dUydyC) );

  hforce.reshape(Nfp, Nc);
  vforce.reshape(Nfp, Nc);
  sJC   .reshape(Nfp, Nc);

  // compute weights for integrating (1,hforce) and (1,vforce)
  V1D = Vandermonde1D(N, r(Fmask(All,1)));
  MM1D = trans(inv(V1D)) / V1D;

  wv = MM1D.col_sums();
  tv = wv*(sJC.dm(hforce));  Cd=tv.sum();  // Compute drag coefficient
  tv = wv*(sJC.dm(vforce));  Cl=tv.sum();  // Compute lift coefficient

  // Output answers for plotting
  fprintf(fid, "%15.14f %15.14f %15.14f %15.14f;...\n", time, Cd/ra, Cl/ra, dP);
  fflush(fid);

  // LOG report
  if (1==tstep || tstep==Nsteps || !umMOD(tstep,Nreport)) { 
    umLOG(1, "%7d   %6.3lf   %9.5lf  %10.6lf  %9.5lf\n", 
              tstep, time, Cd/ra, Cl/ra, dP);
  }

  if (tstep==Nsteps) {
    fprintf(fid, "];\n");
    fclose(fid); fid=NULL;
  }
}

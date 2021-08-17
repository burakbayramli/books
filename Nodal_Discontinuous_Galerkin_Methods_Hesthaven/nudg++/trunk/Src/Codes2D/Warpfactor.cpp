// Warpfactor.m
// function warp = Warpfactor(N, rout)
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"


//---------------------------------------------------------
DVec& Warpfactor(int N, const DVec& rout_arg)
//---------------------------------------------------------
{
  DVec rout(rout_arg);  // transfers ownership of temp arg

  // function warp = Warpfactor(N, rout)
  // Purpose  : Compute scaled warp function at order N 
  //            based on rout interpolation nodes

  int Nr = rout.size(); DMat Pmat(N+1,Nr), Lmat, Veq;
  DVec LGLr, req, zerof, sf; int i=0;
  // Note: create as "OBJ_real" to avoid deletion, then adjust below
  DVec *ret = new DVec("warp", OBJ_real); DVec& warp=(*ret); // "reference"

  // Compute LGL and equidistant node distribution
  LGLr = JacobiGL(0,0,N); req.linspace(-1.0, 1.0, N+1);

  // Compute V based on req
  Veq = Vandermonde1D(N,req);

  // Evaluate Lagrange polynomial at rout
  for (i=1; i<=(N+1); ++i) {
    Pmat.set_row(i, JacobiP(rout, 0, 0, i-1));
  }
  Lmat = trans(Veq)|Pmat;

  // Compute warp factor
  warp = trans(Lmat)*(LGLr - req);

  // Scale factor
  zerof = rout.lt_abs(1.0 - 1e-10); sf = 1.0 - sqr(zerof.dm(rout));
  warp = warp.dd(sf) + warp.dm(zerof-1.0);

  warp.set_mode(OBJ_temp);  // adjust mode flag
  return warp;
}

// evalwarp.cpp
// function warp = evalwarp(p, xnodes, xout)
// 2007/06/11
//---------------------------------------------------------
#include "NDGLib_headers.h"


//---------------------------------------------------------
DVec& evalwarp(int p, const DVec& xnodes, const DVec& xout)
//---------------------------------------------------------
{
  // function warp = evalwarp(p, xnodes, xout)
  // Purpose: compute one-dimensional edge warping function

  DVec* warp = new DVec(xout.size(), 0.0, OBJ_temp, "warp");
  DVec xeq(p+1); int i=0, j=0;

  for (i=1; i<=(p+1); ++i) {
    xeq(i) = -1.0 + 2.0*(p+1.0-i)/double(p);
  }

  DVec d(xout.size());
  for (i=1; i<=(p+1); ++i) {
    d = (xnodes(i)-xeq(i));
    for (j=2; j<=p; ++j) {
      if (i!=j) {
	    //d = d*(xout-xeq(j))/(xeq(i)-xeq(j));
        d *= ((xout-xeq(j)) / (xeq(i)-xeq(j)));
      }
    }
    
    if (i!=   1 ) { d = -d/(xeq(i)-xeq(1  )); }
    if (i!=(p+1)) { d =  d/(xeq(i)-xeq(p+1)); }

    (*warp) += d;
  }

  return (*warp);
}

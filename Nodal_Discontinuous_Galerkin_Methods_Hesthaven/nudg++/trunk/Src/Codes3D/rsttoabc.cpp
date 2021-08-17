// rsttoabc.m
// function [a,b,c] = rsttoabc(r,s,t)
// 2007/06/11
//---------------------------------------------------------
#include "NDGLib_headers.h"


//---------------------------------------------------------
void rsttoabc
(
  const DVec& r,    // [in]
  const DVec& s,    // [in]
  const DVec& t,    // [in]
        DVec& a,    // [out]
        DVec& b,    // [out]
        DVec& c     // [out]
)
//---------------------------------------------------------
{
  // function [a,b,c] = rsttoabc(r,s,t)
  // Purpose: Transfer from (r,s,t) -> (a,b,c) coordinates in triangle  

  int Np = r.size(); a.resize(Np); b.resize(Np);
  for (int n=1; n<=Np; ++n) {
    if (s(n)+t(n) != 0.0) {
      a(n) = 2.0*(1.0+r(n))/(-s(n)-t(n))-1.0;
    } else {
      a(n) = -1.0;
    }
    if (t(n) != 1.0) {
      b(n) = 2.0*(1.0+s(n))/(1.0-t(n))-1.0;
    } else {
      b(n) = -1.0;
    }
  }
  c = t;
  return;
}

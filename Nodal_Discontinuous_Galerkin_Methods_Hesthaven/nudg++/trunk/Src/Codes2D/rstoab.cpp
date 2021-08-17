// rstoab.m
// function [a,b] = rstoab(r,s)
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"


//---------------------------------------------------------
void rstoab
(
  const DVec& r,    // [in]
  const DVec& s,    // [in]
        DVec& a,    // [out]
        DVec& b     // [out]
)
//---------------------------------------------------------
{
  // function [a,b] = rstoab(r,s)
  // Purpose : Transfer from (r,s) -> (a,b) coordinates in triangle

  int Np = r.size(); a.resize(Np);
  for (int n=1; n<=Np; ++n) {
    if (s(n) != 1.0) {
      a(n) = 2.0*(1.0+r(n))/(1.0-s(n))-1.0;
    } else { 
      a(n) = -1.0;
    }
  }
  b = s;
}

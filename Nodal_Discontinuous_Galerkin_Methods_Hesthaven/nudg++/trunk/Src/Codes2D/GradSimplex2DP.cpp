// GradSimplex2DP.cpp
// function [dmodedr, dmodeds] = GradSimplex2DP(a,b,id,jd)
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"


//---------------------------------------------------------
void GradSimplex2DP
(
  const DVec& a,        // [in]
  const DVec& b,        // [in]
        int   id,       // [in]
        int   jd,       // [in]
        DVec& dmodedr,  // [out]
        DVec& dmodeds   // [out]
)
//---------------------------------------------------------
{
  // function [dmodedr, dmodeds] = GradSimplex2DP(a,b,id,jd)
  // Purpose: Return the derivatives of the modal basis (id,jd)
  //          on the 2D simplex at (a,b).

  DVec fa, dfa, gb, dgb, tmp;

  fa = JacobiP(a, 0, 0, id);     dfa = GradJacobiP(a, 0, 0, id);
  gb = JacobiP(b, 2*id+1,0, jd); dgb = GradJacobiP(b, 2*id+1,0, jd);

  // r-derivative
  // d/dr = da/dr d/da + db/dr d/db = (2/(1-s)) d/da = (2/(1-b)) d/da
  dmodedr = dfa.dm(gb);
  if (id>0) {
    dmodedr *= pow(0.5*(1.0-b), (id-1.0));
  }

  // s-derivative
  // d/ds = ((1+a)/2)/((1-b)/2) d/da + d/db
  dmodeds = dfa.dm(gb.dm(0.5*(1.0+a)));
  if (id>0) {
    dmodeds *= pow(0.5*(1.0-b), (id-1.0));
  }

  tmp = dgb.dm(pow(0.5*(1.0-b), double(id)));
  if (id>0) {
    tmp -= (0.5*id)*gb.dm(pow(0.5*(1.0-b), (id-1.0)));
  }
  dmodeds += fa.dm(tmp);

  // Normalize
  dmodedr *= pow(2.0, (id+0.5)); dmodeds *= pow(2.0, (id+0.5));
}

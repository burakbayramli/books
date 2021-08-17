// GradSimplex3DP.m
// function [V3Dr, V3Ds, V3Dt] = GradSimplex3DP(a,b,c,id,jd,kd)
// 2007/06/12
//---------------------------------------------------------
#include "NDGLib_headers.h"


//---------------------------------------------------------
void GradSimplex3DP
(
  const DVec& a,      // [in]
  const DVec& b,      // [in]
  const DVec& c,      // [in]
        int   id,     // [in]
        int   jd,     // [in]
        int   kd,     // [in]
        DVec& V3Dr,   // [out]
        DVec& V3Ds,   // [out]
        DVec& V3Dt    // [out]
)
//---------------------------------------------------------
{
  // function [V3Dr, V3Ds, V3Dt] = GradSimplex3DP(a,b,c,id,jd,kd)
  // Purpose: Return the derivatives of the modal basis (id,jd,kd) 
  //          on the 3D simplex at (a,b,c)

  DVec fa, dfa, gb, dgb, hc, dhc, tmp;


  fa = JacobiP(a,0,0,id);           dfa = GradJacobiP(a,0,0,id);
  gb = JacobiP(b,2*id+1,0,jd);      dgb = GradJacobiP(b,2*id+1,0,jd);
  hc = JacobiP(c,2*(id+jd)+2,0,kd); dhc = GradJacobiP(c,2*(id+jd)+2,0,kd);

  // r-derivative
  V3Dr = dfa.dm(gb.dm(hc));
  if (id>0)    { V3Dr *= pow(0.5*(1.0-b), double(id-1)); }
  if (id+jd>0) { V3Dr *= pow(0.5*(1.0-c), double(id+jd-1)); }

  // s-derivative 
  V3Ds = V3Dr.dm(0.5*(1.0+a));
  tmp = dgb.dm(pow(0.5*(1.0-b), double(id)));

  if (id>0)    { tmp -= (0.5*id) * gb.dm(pow(0.5*(1.0-b), (id-1.0))); }
  if (id+jd>0) { tmp *= pow(0.5*(1.0-c),(id+jd-1.0)); }
  tmp = fa.dm(tmp.dm(hc));
  V3Ds += tmp;

  // t-derivative 
  V3Dt = V3Dr.dm(0.5*(1.0+a)) + tmp.dm(0.5*(1.0+b));
  tmp = dhc.dm(pow(0.5*(1.0-c), double(id+jd)));
  if (id+jd>0) { tmp -= (0.5*(id+jd)) * hc.dm(pow(0.5*(1.0-c), (id+jd-1.0))); }
  tmp = fa.dm(gb.dm(tmp));
  tmp *= pow(0.5*(1.0-b), double(id));
  V3Dt += tmp;

  // normalize
  double fac = pow(2.0, (2.0*id + jd+1.5));
  V3Dr *= fac;  V3Ds *= fac;  V3Dt *= fac;
}

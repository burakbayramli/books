// GradVandermonde3D.m
// function [V3Dr,V3Ds,V3Dt] = GradVandermonde3D(N,r,s,t)
// 2007/06/12
//---------------------------------------------------------
#include "NDGLib_headers.h"


//---------------------------------------------------------
void GradVandermonde3D
(
        int   N,      // [in]
  const DVec& r,      // [in]
  const DVec& s,      // [in]
  const DVec& t,      // [in]
        DMat& V3Dr,   // [out]
        DMat& V3Ds,   // [out]
        DMat& V3Dt    // [out]
)
//---------------------------------------------------------
{
  // function [V3Dr,V3Ds,V3Dt] = GradVandermonde3D(N,r,s,t)
  // Purpose : Initialize the gradient of the modal basis (i,j,k)
  //		at (r,s,t) at order N

  int Nr=r.size(), Nc=(N+1)*(N+2)*(N+3)/6;
  V3Dr.resize(Nr,Nc); V3Ds.resize(Nr,Nc); V3Dt.resize(Nr,Nc);
  DVec a,b,c, ddr,dds,ddt; 

  // find tensor-product coordinates
  rsttoabc(r,s,t, a,b,c);

  // Initialize matrices
  int sk = 1;
  for (int i=0; i<=N; ++i) {
    for (int j=0; j<=(N-i); ++j) {
      for (int k=0; k<=(N-i-j); ++k) {
        GradSimplex3DP(a,b,c,i,j,k, ddr,dds,ddt);
        V3Dr(All,sk)=ddr; V3Ds(All,sk)=dds; V3Dt(All,sk)=ddt;
        ++sk;
      }
    }
  }
}

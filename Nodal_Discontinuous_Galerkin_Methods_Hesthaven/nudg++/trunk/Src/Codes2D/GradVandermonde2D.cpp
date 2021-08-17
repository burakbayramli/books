// GradVandermonde2D.m
// function [V2Dr,V2Ds] = GradVandermonde2D(N,r,s)
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"


//---------------------------------------------------------
void GradVandermonde2D
(
        int   N,      // [in]
  const DVec& r,      // [in]
  const DVec& s,      // [in]
        DMat& V2Dr,   // [out]
        DMat& V2Ds    // [out]
)
//---------------------------------------------------------
{
  // function [V2Dr,V2Ds] = GradVandermonde2D(N,r,s)
  // Purpose : Initialize the gradient of the modal basis (i,j)
  //		at (r,s) at order N

  DVec a,b, ddr,dds;
  V2Dr.resize(r.size(), (N+1)*(N+2)/2);
  V2Ds.resize(r.size(), (N+1)*(N+2)/2);

  // find tensor-product coordinates
  rstoab(r,s, a,b);

  // Initialize matrices
  int sk = 1;
  for (int i=0; i<=N; ++i) {
    for (int j=0; j<=(N-i); ++j) {
      GradSimplex2DP(a,b,i,j, ddr,dds); 
      V2Dr(All,sk)=ddr; V2Ds(All,sk)=dds;
      ++sk;
    }
  }
}

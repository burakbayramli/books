// Nodes2D.m
// function [x,y] = Nodes2D(N);
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"


//---------------------------------------------------------
void Nodes2D(int N, DVec& x, DVec& y)
//---------------------------------------------------------
{
  // function [x,y] = Nodes2D(N);
  // Purpose  : Compute (x,y) nodes in equilateral triangle for
  //            polynomial of order N

  DVec blend1,blend2,blend3,warp1,warp2,warp3,warpf1,warpf2,warpf3;
  DVec alpopt(gVecData, 15, 
        " 0.0000 0.0000 1.4152 0.1001 0.2751 " 
        " 0.9800 1.0999 1.2832 1.3648 1.4773 "
        " 1.4959 1.5743 1.5770 1.6223 1.6258");
            
  // Set optimized parameter, alpha, depending on order N
  double alpha = 5.0/3.0;
  if (N<16) {
    alpha = alpopt(N);
  }

  // total number of nodes
  int Np = (N+1)*(N+2)/2;  double NN = double(N);

  // Create equidistributed nodes on equilateral triangle
  DVec L1(Np), L2(Np), L3(Np);

  int sk = 1;
  for (int n=1; n<=(N+1); ++n) {
    for (int m=1; m<=(N+2-n); ++m) {
      L1(sk) = (n-1)/NN; L3(sk) = (m-1)/NN;
      ++sk;
    }
  }

  L2 = 1.0 - L1 - L3;
  
  x =  -L2+L3; 
  y = (-L2-L3 + 2.0*L1)/sqrt(3.0);

  // Compute blending function at each node for each edge
  blend1 = 4.0 * L2.dm(L3);
  blend2 = 4.0 * L1.dm(L3);
  blend3 = 4.0 * L1.dm(L2);

  // Amount of warp for each node, for each edge
  warpf1 = Warpfactor(N, L3-L2);
  warpf2 = Warpfactor(N, L1-L3); 
  warpf3 = Warpfactor(N, L2-L1);

  // Combine blend & warp
  warp1 = blend1.dm(warpf1);  warp1 *= (1.0 + sqr(alpha*L1));
  warp2 = blend2.dm(warpf2);  warp2 *= (1.0 + sqr(alpha*L2));
  warp3 = blend3.dm(warpf3);  warp3 *= (1.0 + sqr(alpha*L3));

  // Accumulate deformations associated with each edge
  x += (1.0*warp1 + cos(2.0*PI/3.0)*warp2 + cos(4.0*PI/3.0)*warp3);
  y += (0.0*warp1 + sin(2.0*PI/3.0)*warp2 + sin(4.0*PI/3.0)*warp3);
}

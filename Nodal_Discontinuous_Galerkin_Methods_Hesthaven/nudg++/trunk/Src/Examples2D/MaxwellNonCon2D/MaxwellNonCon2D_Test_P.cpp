// MaxwellNonCon2D_Test_P.cpp
// 
// 2007/08/04
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "MaxwellNonCon2D.h"


//---------------------------------------------------------
void MaxwellNonCon2D::AdjustMesh_P()
//---------------------------------------------------------
{
  umMSG(1, "Adjusting mesh for non-conforming (P) elements\n");

  Nfaces = 3;
  tiConnect2D(EToV, EToE,EToF);

  // make boundary conditions all "Wall" type
  BCType = int(BC_Wall) * EToE.eq(outer(Range(1,K),Ones(Nfaces)));

  IVec Norder(K);

  if (1) {
    // generate a random order for each element
    Norder = ceil(10.0*rand(K));
  } else if (0) {
    Norder = 5;
  } else {
    Norder(1) = 1;
    Norder(2) = 1;
    Norder(3) = 2;
    Norder(4) = 2;
    Norder(5) = 3;
    Norder(6) = 3;
    Norder(Range(7,K)) = 4;
  }

  // Build mesh, each element having arbitrary order
  BuildPNonCon2D(Norder, K, VX, VY, EToV, BCType,  m_PInfo);

  xx.resize(max_pinf_id);
  yy.resize(max_pinf_id);
  for (int N1=1; N1<=Nmax; ++N1) {
    const PInfo& pinf = (*m_PInfo[N1]);
    if (pinf.K > 0) {
      const IVec& pids = dynamic_cast<const IVec&>(pinf.ids);
      xx(pids) = pinf.x;
      yy(pids) = pinf.y;
    }
  }
}

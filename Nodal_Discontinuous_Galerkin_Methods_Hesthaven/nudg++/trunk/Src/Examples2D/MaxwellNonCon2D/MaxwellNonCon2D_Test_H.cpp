// MaxwellNonCon2D_Test_H.cpp
// 
// 2007/08/04
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "MaxwellNonCon2D.h"


//---------------------------------------------------------
void MaxwellNonCon2D::AdjustMesh_H()
//---------------------------------------------------------
{
  umMSG(1, "Adjusting mesh for non-conforming (H) elements\n");

  // make boundary conditions all "Wall" type
  BCType = int(BC_Wall) * EToE.eq(outer(Range(1,K),Ones(Nfaces)));

  //--------------------------------------------------
  // create a non-conforming interface by refinement
  //--------------------------------------------------
  IVec refineflag(K);

  // 1st test refinement
  refineflag(Range(1,5)) = 1;   // select elements to refine
  Hrefine2D(refineflag);        // refine elements {1:5}
  StartUp2D();                  // rebuild grid and metric
  BuildBCMaps2D();              // map boundary faces

  // 2nd test refinement
  refineflag.resize(K,true,0);  // extend and clear flag array
  refineflag(K) = 1;            // select elements to refine
  Hrefine2D(refineflag);        // refine element {K}
  StartUp2D();                  // rebuild grid and metric
  BuildBCMaps2D();              // map boundary faces

  // build face (neighbor) info
  BuildHNonCon2D(N+1, 1e-6, m_FInfo);
}

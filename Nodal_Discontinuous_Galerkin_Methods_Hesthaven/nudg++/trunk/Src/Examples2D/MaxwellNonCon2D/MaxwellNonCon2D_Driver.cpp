// MaxwellNonCon2D_Driver.cpp
// 
// 2007/08/04
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "MaxwellNonCon2D.h"


//---------------------------------------------------------
void MaxwellNonCon2D::Driver()
//---------------------------------------------------------
{
  //-------------------------------------
  // select test mode
  //-------------------------------------
//noncon_mode = eModeH;
  noncon_mode = eModeP;

  //-------------------------------------
  // Set polynomial order
  //-------------------------------------
//N = 20;
//N = 12;
//N = 7;
  N = 4;

  if (eModeH == noncon_mode) {
    FileName = "Grid/Maxwell2D/Maxwell05.neu";
    FinalTime = sqrt(2.0);    // Solve for exactly one period
  } else {
    FileName = "Grid/Maxwell2D/Maxwell025.neu";
    FinalTime = sqrt(2.0);    // Solve for exactly one period
  //FinalTime = 1.0;
  }

  //-------------------------------------
  // Read in Mesh: vert, elmt, mat, BC
  //-------------------------------------
  if (!MeshReaderGambit2D(FileName)) {
    umWARNING("Maxwell2D::Driver", "Error loading mesh (file: %s)\nExiting.\n", FileName.c_str());
    return;
  }

  // store origial number of elements
  mesh_K = K;

  try {
    // call virtual base class Run()
    Run();
  } catch (...) {
    umWARNING("MaxwellNonCon2D::Driver", "Caught exception from Run()");
    return;
  }
}


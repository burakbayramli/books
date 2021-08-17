// MaxwellCurved2D_Driver.cpp
// 
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "MaxwellCurved2D.h"


//---------------------------------------------------------
void MaxwellCurved2D::Driver()
//---------------------------------------------------------
{
  umLOG(1, "MaxwellCurved2D::Driver()\n");

  // Set polynomial order to use
  N = 8;

  // Read and initiate circular mesh
  FileName = "Grid/Other/circA01.neu";

  if (!MeshReaderGambit2D(FileName)) {
    umWARNING("MaxwellCurved2D::Driver", "Error loading mesh (file: %s)\nExiting.\n", FileName.c_str());
    return;
  }

  try {
    // Solve Problem for exactly one period
    FinalTime = 0.1;
    Run();
  } catch (...) {
    umWARNING("MaxwellCurved2D::Driver", "Caught exception from Run()");
    return;
  }
}

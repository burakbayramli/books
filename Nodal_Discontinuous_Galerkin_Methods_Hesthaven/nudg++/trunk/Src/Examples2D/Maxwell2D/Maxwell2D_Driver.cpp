// Maxwell2D_Driver.cpp
// Driver for solving the 2D vacuum Maxwell's equations on TM form
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "Maxwell2D.h"


//---------------------------------------------------------
void Maxwell2D::Driver()
//---------------------------------------------------------
{
  umLOG(1, "Maxwell2D::Driver()\n");

  // Polynomial order used for approximation 
//N =  3;
  N =  8;
//N = 10;

//FileName = "Grid/Maxwell2D/Maxwell00625.neu";
//FileName = "Grid/Maxwell2D/Maxwell0125.neu";
  FileName = "Grid/Maxwell2D/Maxwell025.neu";
//FileName = "Grid/Maxwell2D/Maxwell05.neu";
//FileName = "Grid/Maxwell2D/Maxwell1.neu";
//FileName = "Grid/Maxwell2D/Maxwell2.neu";
//FileName = "Grid/Maxwell2D/maxmesh.neu";
//FileName = "Grid/Other/circA01.neu";

  // Read in Mesh: [vertices, elements, materials, BC's]
  if (!MeshReaderGambit2D(FileName)) {
    umWARNING("Maxwell2D::Driver", "Error loading mesh (file: %s)\nExiting.\n", FileName.c_str());
    return;
  }

  try {
    // Solve Problem
  //FinalTime = 2*pi;
    FinalTime = 1.0;
    Run();
  } catch (...) {
    umWARNING("Maxwell2D::Driver", "Caught exception from Run()");
    return;
  }
}

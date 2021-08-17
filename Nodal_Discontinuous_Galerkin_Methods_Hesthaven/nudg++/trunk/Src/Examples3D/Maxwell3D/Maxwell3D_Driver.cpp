// Maxwell3D_Driver.cpp
// Driver for solving the 3D vacuum Maxwell's equations
// 2007/06/11
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "Maxwell3D.h"


//---------------------------------------------------------
void Maxwell3D::Driver()
//---------------------------------------------------------
{
  umLOG(1, "Maxwell3D::Driver()\n");

  // Polynomial order of approximation 
//N = 9;
//N = 4;
//N = 5;
//N = 6;
//N = 7;
  N = 8;

//FileName = "Grid/3D/cubeK6.neu";
  FileName = "Grid/3D/cubeK268.neu";
//FileName = "Grid/Euler3D/C_16_12_1574.neu";
//FileName = "Grid/Euler3D/C_18_16_1469.neu";
//FileName = "Grid/Euler3D/C_42_64_3200.neu";

  // Read in Mesh: [vertices, elements, materials, BC's]
  if (!MeshReaderGambit3D(FileName)) {
    umWARNING("Maxwell3D::Driver", "Error loading mesh (file: %s)\nExiting.\n", FileName.c_str());
    return;
  }

  try {
    // Solve Problem
  //FinalTime = 10.0;
  //FinalTime =  1.0;
    FinalTime =  0.1;
    Run();
  } catch (...) {
    umWARNING("Maxwell3D::Driver", "Caught exception from Run()");
    return;
  }
}

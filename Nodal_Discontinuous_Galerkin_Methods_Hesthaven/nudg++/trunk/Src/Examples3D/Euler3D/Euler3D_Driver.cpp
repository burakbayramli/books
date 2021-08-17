// Euler3D_Driver.cpp
// Driver script for solving the 3D Euler equations
// 2007/07/11
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "Euler3D.h"


//---------------------------------------------------------
void Euler3D::Driver()
//---------------------------------------------------------
{
  umLOG(1, "Euler3D::Driver()\n");

  // Order of polynomials used for approximation 
  N = 4;
//N = 6;
  N = 8;

  sim_type = eIsentropicVortex;
//sim_type = eCouetteFlow;

  switch (sim_type) {
  case eIsentropicVortex:
  //FileName = "Grid/3D/cubeK6.neu";
    FileName = "Grid/3D/cubeK268.neu";
    InitialSolution = &Euler3D::IsentropicVortexIC3D;
    ExactSolution   = &Euler3D::IsentropicVortexIC3D;
    BCSolution      = &Euler3D::IsentropicVortexBC3D;
    m_bApplyFilter  = false;   // toggle use of filter
    break;

  case eCouetteFlow:
  //FileName = "Grid/Euler3D/C_18_16_1469.neu";
    FileName = "Grid/Euler3D/C_42_64_3200.neu";
    InitialSolution = &Euler3D::CouetteIC3D;
    ExactSolution   = &Euler3D::CouetteIC3D;
    BCSolution      = &Euler3D::CouetteBC3D;
    m_bApplyFilter  = true;   // toggle use of filter
    break;


  default:
    umERROR("Euler3D::Driver()", "Simulation case unknown");
    return;
  }

  // Read in Mesh: [vertices, elements, materials, BC's]
  if (!MeshReaderGambit3D(FileName)) {
    umWARNING("Euler3D::Driver", "Error loading mesh (file: %s)\nExiting.\n", FileName.c_str());
    return;
  }

  // VX = VX*5; VY = VY*5; VZ = VZ*5;

  try {
    // Solve Problem
  //FinalTime =  5.0;
    FinalTime =  0.1;
    Run();
  } catch (...) {
    umWARNING("Euler3D::Driver", "Caught exception from Run()");
    return;
  }
}

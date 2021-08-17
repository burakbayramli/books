// CurvedCNS2D_Driver.m
// Driver script for solving the 2D compressible Navier-Stokes equations
// 2007/06/24
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedCNS2D.h"


//---------------------------------------------------------
void CurvedCNS2D::Driver()
//---------------------------------------------------------
{
  //--------------------------------------------------
  // select order of polynomial approximation (N) 
  //--------------------------------------------------
//N = 3;
//N = 4;
//N = 5;
  N = 6;
//N = 7;
//N = 8;
//N = 9;
//N = 10;

  //--------------------------------------------------
  // select simulation type
  //--------------------------------------------------
//sim_type = eCylinderFlow;
  sim_type = eChannelFlow;
//sim_type = eBoxFlow;
//sim_type = eCouetteFlow;

  //--------------------------------------------------
  // select flux type
  //--------------------------------------------------
//flux_type = FT_LaxF;
  flux_type = FT_Roe;
//flux_type = FT_HLL;


  //--------------------------------------------------
  // select mesh, initial conditions, and BC function
  //--------------------------------------------------

  switch (sim_type) {
  case eCylinderFlow:
  //FileName        = "Grid/CFD/cylinderA00075b.neu";
    FileName        = "Grid/CFD/Volker_374.neu";
    InitialSolution = &CurvedCNS2D::CylIC2D;
    ExactSolution   = NULL;
    BCSolution      = &CurvedCNS2D::CylBC2D;
    break;

  case eChannelFlow:
    FileName        = "Grid/CNS2D/otboxA01.neu";
    InitialSolution = &CurvedCNS2D::ChannelIC2D;
    ExactSolution   = &CurvedCNS2D::ChannelIC2D;
    BCSolution      = &CurvedCNS2D::ChannelBC2D;
    break;

  case eBoxFlow:
    FileName        = "Grid/CNS2D/otboxA01.neu";
    InitialSolution = &CurvedCNS2D::BoxFlowIC2D;
    ExactSolution   = NULL;
    BCSolution      = &CurvedCNS2D::BoxFlowBC2D;
    break;

  case eCouetteFlow:
  //FileName        = "Grid/CNS2D/Couette_K082.neu";
  //FileName        = "Grid/CNS2D/Couette_K242.neu";
  //FileName        = "Grid/CNS2D/Couette_K856.neu";
    FileName        = "Grid/CNS2D/C_1_2_K1662.neu";
  //FileName        = "Grid/CNS2D/Couette_K1514_8Balls.neu";
  //FileName        = "Grid/CNS2D/Couette_K2600_8Balls.neu";
  //FileName        = "Grid/CNS2D/Couette_K5003_8Balls.neu";
    InitialSolution = &CurvedCNS2D::CouetteIC2D;
    ExactSolution   = &CurvedCNS2D::CouetteIC2D;
    BCSolution      = &CurvedCNS2D::CouetteBC2D;
    break;

  default:
    umERROR("CurvedCNS2D::Driver()", "Simulation case unknown");
    break;
  }


  //--------------------------------------------------
  // read mesh: vertices, elements, materials, BC's
  //--------------------------------------------------
  if (!MeshReaderGambit2D(FileName)) {
    umWARNING("CurvedCNS2D::Driver", "Error loading mesh (file: %s)\nExiting.\n", FileName.c_str());
    return;
  }

  try {
  //FinalTime =  0.005;
    FinalTime =  15.0;
  //FinalTime =  0.5;
  //FinalTime = 50.0;
    Run();  // Solve Problem
  } catch (...) {
    umWARNING("CurvedCNS2D::Driver", "Caught exception from Run()");
    return;
  }
}

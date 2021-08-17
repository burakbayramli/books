// CurvedINS2D_Driver.cpp
// Driver for computing the solution of the
// incompressible Navier-Stokes equations
// 2007/07/23
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedINS2D.h"


//---------------------------------------------------------
void CurvedINS2D::Driver()
//---------------------------------------------------------
{
  umLOG(1, "CurvedINS2D::Driver()\n");

  //-------------------------------------
  // Choose simulation type, domain, 
  // initial solution and BC functions
  //-------------------------------------
//sim_type = eChannel;
//sim_type = eKovasznay;
//sim_type = eStep;
  sim_type = eVolkerCylinder;
//sim_type = ePearsonVortex;
//sim_type = eStirer;
//sim_type = eBackdrop;
//sim_type = eWedge;

  switch (sim_type) {

  case eChannel:
  //FileName        = "Grid/CFD/channelA1.neu";
    FileName        = "Grid/CFD/cylinderA00075b.neu";
    ExactSolution   = &CurvedINS2D::INSchannelIC2D;
    ExactSolutionBC = &CurvedINS2D::INSchannelBC2D;
    FinalTime = 1.0;  nu = 1.0/40.0;
    break;

  case eKovasznay:
    FileName        = "Grid/CFD/kovA02.neu";
    ExactSolution   = &CurvedINS2D::KovasznayIC2D;
    ExactSolutionBC = &CurvedINS2D::KovasznayBC2D;
    FinalTime = 1.0;  nu = 1.0/40.0;
    break;

  case eVolkerCylinder:
  //FileName        = "Grid/CFD/cylinderA00075b.neu";
  //FileName        = "Grid/CFD/cylinderCA0015.neu";
  //FileName        = "Grid/CFD/Volker_306.neu";
    FileName        = "Grid/CFD/Volker_374.neu";
    ExactSolution   = &CurvedINS2D::INScylinderIC2D;
    ExactSolutionBC = &CurvedINS2D::INScylinderBC2D;
    FinalTime = 8.0;  nu = 1e-3;
    break;

  case ePearsonVortex:
  //FileName        = "Grid/CFD/pvortex4A01.neu";
  //FileName        = "Grid/CFD/pv_0014.neu";
  //FileName        = "Grid/CFD/pv_0036.neu";
  //FileName        = "Grid/CFD/pv_0080.neu";
  //FileName        = "Grid/CFD/pv_0156.neu";
  //FileName        = "Grid/CFD/pv_0300.neu";
    FileName        = "Grid/CFD/pv_0600.neu";
  //FileName        = "Grid/CFD/pv_0999.neu";
    ExactSolution   = &CurvedINS2D::PearsonVortexIC2D;
    ExactSolutionBC = &CurvedINS2D::PearsonVortexBC2D;
    FinalTime = 0.1;  nu = 1e-2;
    break;

  case eStirer:
    break;

  case eBackdrop:
    break;

  case eWedge:
    break;
  }

  //---------------------------------------------
  // Order of polynomial approximation (N)
  //---------------------------------------------
//N =  5;
  N =  8;    // Note: Volker_374.neu - very coarse at outflow!
//N = 12;

//FinalTime = 1.0;
  FinalTime = 8.0;
//FinalTime = 0.005;

  // Read in Mesh: [vertices, elements, materials, BC's]
  if (!MeshReaderGambit2D(FileName)) {
    umWARNING("CurvedINS2D::Driver", "Error loading mesh (file: %s)\nExiting.\n", FileName.c_str());
    return;
  }

  try {
    Run();  // Solve Problem
  } catch (...) {
    umWARNING("CurvedINS2D::Driver", "Caught exception from Run()");
    return;
  }
}

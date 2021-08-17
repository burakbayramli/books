// EulerShock2D_Driver.m
// Driver script for solving the 2D vacuum Euler's equations 
// specialization of CurvedEuler2D for handling shocks.
// 2007/08/15
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "EulerShock2D.h"


//---------------------------------------------------------
void EulerShock2D::Driver()
//---------------------------------------------------------
{
  //--------------------------------------------------
  // select order of polynomial approximation (N) 
  //--------------------------------------------------
  N = 1;

  //--------------------------------------------------
  // select number of h-refinements of default mesh
  //--------------------------------------------------
  Nrefine = 0;    // no global refinement

//Nrefine = 1;    // K:  381 ->  1524  (fstepA001.neu)
//Nrefine = 2;    // K: 1524 ->  6096
//Nrefine = 3;    // K: 6096 -> 24384

  //--------------------------------------------------
  // select simulation type
  //--------------------------------------------------
//sim_type = eForwardStep;
  sim_type = eScramInlet;

  //--------------------------------------------------
  // select flux type
  //--------------------------------------------------
  flux_type = FT_Roe;
//flux_type = FT_HLL;
//flux_type = FT_Eigen;

  //--------------------------------------------------
  // select mesh, initial conditions, and BC function
  //--------------------------------------------------

  switch (sim_type) {

  case eForwardStep:
    FileName        = "Grid/Euler2D/fstepA001.neu";
    InitialSolution = (fp_IC)(&EulerShock2D::ForwardStepIC2D);
    ExactSolution   = NULL;
    BCSolution      = (fp_BC)(&EulerShock2D::ForwardStepBC2D);
    break;

  case eScramInlet:
    FileName        = "Grid/Euler2D/Scram_0977.neu";
    InitialSolution = (fp_IC)(&EulerShock2D::InletIC2D);
    ExactSolution   = NULL;
    BCSolution      = (fp_BC)(&EulerShock2D::InletBC2D);
    break;


  // See http://www.lcp.nrl.navy.mil/cfd-cta/CFD3/problems/inlet/index.html

  default:
    umERROR("EulerShock2D::Driver()", "Simulation case not implemented");
    return;
  }

  //--------------------------------------------------
  // read mesh: vertices, elements, materials, BC's
  //--------------------------------------------------
  if (!MeshReaderGambit2D(FileName)) {
    umWARNING("EulerShock2D::Driver", "Error loading mesh (file: %s)\nExiting.\n",FileName.c_str());
    return;
  }

  try {

    switch (sim_type) {
    case eForwardStep:
      FinalTime = 4.0;
      Run();          // Solve Problem
      break;

    case eScramInlet:
      Run_Iter();     // "iterative" Run()
      break;
    }

  } catch (...) {
    umWARNING("EulerShock2D::Driver", "Caught exception from Run()");
    return;
  }
}

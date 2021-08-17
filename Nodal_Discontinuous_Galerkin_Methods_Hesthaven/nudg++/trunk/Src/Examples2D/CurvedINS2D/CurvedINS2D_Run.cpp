// CurvedINS2D.m
// function [Ux, Uy, PR, time] = CurvedINS2D(...
//           Ux, Uy, PR, FinalTime, nu, simtype, ExactSolution, BCfunction);
// 2007/06/10
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedINS2D.h"


//---------------------------------------------------------
void CurvedINS2D::Run()
//---------------------------------------------------------
{
  ti0=timer.read();

  // function [Ux, Uy, PR, time] = CurvedINS2D(...
  //           Ux, Uy, PR, FinalTime, nu, simtype, ExactSolution, BCfunction);
  // Purpose: integrate the incompressible Navier-Stokes equations to FinalTime

  InitRun();            // initialize timers & counters

  // choose order to integrate exactly
  int Nint = (int)ceil(3.0*N/2.0);

  // build cubature nodes for all elements
  CubatureOrder = 2*(Nint+1); CubatureVolumeMesh2D(CubatureOrder);

  // build Gauss node data for all element faces
  NGauss = (Nint+1); GaussFaceMesh2D(NGauss);

#if (1)
  //-------------------------------------
  // check all node sets
  //-------------------------------------
  Output_Mesh();
//OutputNodes(false);   // volume nodes
//OutputNodes(true);    // face nodes
//OutputNodes_cub();    // cubature
//OutputNodes_gauss();  // quadrature
//umLOG(1, "\n*** Exiting after CUb, Gauss\n\n");
//return;
#endif

  // recover memory from registry
  NDG_garbage_collect();

  // prepare data...
  PreCalcBdryData();

  // dual splitting scheme coefficients
  g0 = 1.0; a0 = 1.0; a1 = 0.0; b0 = 1.0; b1 = 0.0; 

  CurvedINSPressureSetUp2D(); // Build pressure matrix and boundary forcing (IPDG)
  NDG_garbage_collect();      // recover memory from registry

  CurvedINSViscousSetUp2D();  // Build viscous matrix and boundary forcing (IPDG)
  NDG_garbage_collect();      // recover memory from registry

  (this->*ExactSolutionBC)    // Form inhomogeneous boundary term for rhs data 
          (Fx, Fy, nx,ny, mapI, mapO, mapW, mapC, 0.0, nu, 
           refbcUx, refbcUy, refbcPR, refbcdUndt);


  // storage for history of fields and nonlinear terms
  Uxold=Ux; NUx=0.0;  Uyold=Uy; NUy=0.0; dpdn=0.0; 

  time_work += timer.read() - ti0;  // add cost of NDG setup

  // start time stepping
  time = 0.0;
  for (tstep=1; tstep<=Nsteps; ++tstep)
  {
    tw1=timer.read();   // time NDG work

    // update dual splitting scheme coefficients after 
    // first time step, then recalculate operators
    if (2 == tstep) 
    {
      // release and recreate Cholesky solvers
      reset_solvers();

      g0 = 1.5; a0 = 2.0; a1 = -0.5; b0 = 2.0; b1 = -1.0; 

      // Rebuild pressure and viscous matrixes for new g0
      CurvedINSPressureSetUp2D();
      CurvedINSViscousSetUp2D();

      NDG_garbage_collect();
      umMSG(1, "2nd sparse setup completed\n");
    //umERROR("Testing", "Exiting early");
    }

    TimeScaleBCData();      // apply temporal scaling factors to bc data

    INSAdvection2D();       // compute nonlinear terms NUx, NUy
  //CurvedINSAdvection2D;   // curved version

    INSPressure2D();        // compute pressure PR and intermediate UxTT, UyTT
  //CurvedINSPressure2D;    // curved version

    CurvedINSViscous2D();   // compute viscous solves and update velocity

    time_work += timer.read() - tw1;  // accumulate cost of NDG work
    time = tstep*dt;        // increment time 
    Report();               // report results

    // if (tstep>100) break;
  }

  time_total = timer.read()-ti0;  // stop timing
  FinalReport();                  // final report
}

// CurvedCNS2D.m
// function Q = CurvedCNS2D(Q, FinalTime, ExactSolution, ExactSolutionBC)
// 2007/07/12
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedCNS2D.h"


//---------------------------------------------------------
void CurvedCNS2D::Run()
//---------------------------------------------------------
{
  // function Q = CurvedCNS2D(Q, FinalTime, ExactSolution, ExactSolutionBC)
  // Purpose  : Integrate 2D compressible Navier-Stokes using a 4th order low storage RK

  InitRun();

  CubatureOrder = 3*(N+1);
  NGauss = (int)ceil(double(3*(N+1)/2));

  CubatureVolumeMesh2D(CubatureOrder);  // build cubature information
  GaussFaceMesh2D(NGauss);              // build Gauss node data

  Resize_cub();           // resize cubature arrays
  MapGaussFaceData();     // {nx = gauss.nx}, etc.
  PreCalcBdryData();      // gmapB = concat(mapI, mapO), etc.
  ti0=timer.read();       // time simulation loop


#if (0)
  //-------------------------------------
  // check all node sets
  //-------------------------------------
  OutputNodes(false);   // volume nodes
  OutputNodes(true);    // face nodes
  OutputNodes_cub();    // cubature
  OutputNodes_gauss();  // quadrature
  Report(true);         // show initial conditions
  umLOG(1, "\n*** Exiting after Cub, Gauss\n\n");
  return;
#endif


  // outer time step loop 
  while (time<FinalTime) 
  {
    if (time+dt > FinalTime) { dt = FinalTime-time; }
    tw1=timer.read();   // time NDG work

    for (int INTRK=1; INTRK<=5; ++INTRK) {
      RKtime = time + dt*rk4c(INTRK);

      // compute right hand side of compressible Navier-Stokes equations
      CurvedCNS2D::RHS(RKtime, BCSolution); // -> rhsQ

      // initiate and increment Runge-Kutta residuals
      resQ *= rk4a(INTRK);  resQ += dt*rhsQ;

      // update fields
      Q += rk4b(INTRK)*resQ;  
    }

    time_work += timer.read() - tw1;  // accumulate cost of NDG work
    time += dt;         // increment time
    SetStepSize();      // compute new timestep
    Report();           // optional reporting
    ++tstep;            // increment timestep

    //if (tstep>=10) break;
  }

  time_total = timer.read()-ti0;  // stop timing
  FinalReport();
}

// CurvedEuler2D.m
// function Q = EulerShock2D(Q, FinalTime, ExactSolution, ExactSolutionBC, fluxtype)
// 2007/06/30
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "EulerShock2D.h"


//---------------------------------------------------------
void EulerShock2D::Run()
//---------------------------------------------------------
{
  // function Q = EulerShock2D(Q,FinalTime, ExactSolution, ExactSolutionBC, fluxtype)
  // Purpose  : Integrate 2D Euler equations using a 2nd order SSP Runge-Kutta time integrator

  InitRun();

  // choose order to integrate exactly
  CubatureOrder = (int)floor(2.0*(N+1)*3.0/2.0);
  NGauss        = (int)floor(2.0*(N+1));

  // build cubature node data for all elements
  CubatureVolumeMesh2D(CubatureOrder);

  // build Gauss node data for all element faces
  GaussFaceMesh2D(NGauss);

  Resize_cub();         // resize cubature arrays
//MapGaussFaceData();   // {nx = gauss.nx}, etc.
  ti0=timer.read();     // time simulation loop

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


  // limit initial condition
  Q = EulerLimiter2D(Q, time);

  // outer time step loop 
  while (time<FinalTime) {

    if (time+dt > FinalTime) {dt=FinalTime-time;}
    tw1=timer.read();   // time NDG work
    oldQ = Q;           // store solutuion from previous step

    // 2nd order SSP Runge-Kutta
    this->RHS(Q,  time, BCSolution); Q1 =  Q +      dt*rhsQ;      Q1 = EulerLimiter2D(Q1, time);
    this->RHS(Q1, time, BCSolution); Q  = (Q + Q1 + dt*rhsQ)/2.0; Q  = EulerLimiter2D(Q,  time);
    
    time += dt;         // increment time 
    SetStepSize();      // compute new timestep
    time_work += timer.read() - tw1;  // accumulate cost of NDG work
    Report();           // optional reporting
    ++tstep;            // increment timestep

    // if (tstep>=10) break;      // testing
  }

  time_total = timer.read()-ti0;  // stop timing
  FinalReport();                  // final report
}

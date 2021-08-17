// CurvedEuler2D.m
// function Q = CurvedEuler2D(Q, FinalTime, ExactSolution, ExactSolutionBC, fluxtype)
// 2007/06/21
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedEuler2D.h"


//---------------------------------------------------------
void CurvedEuler2D::Run()
//---------------------------------------------------------
{
  // function Q = CurvedEuler2D(Q, FinalTime, ExactSolution, ExactSolutionBC, fluxtype)
  // Purpose  : Integrate 2D Euler equations using a 4th order low storage RK

  InitRun();

  // choose order to integrate exactly
  CubatureOrder = (int)floor(2.0*(N+1)*3.0/2.0);
  NGauss        = (int)floor(2.0*(N+1));

  // build cubature node data for all elements
  CubatureVolumeMesh2D(CubatureOrder);

  // build Gauss node data for all element faces
  GaussFaceMesh2D(NGauss);

  Resize_cub();           // resize cubature arrays
  MapGaussFaceData();     // {nx = gauss.nx}, etc.
  PreCalcBdryData();      // gmapB = concat(mapI, mapO), etc.
  ti0=timer.read();       // time simulation loop

  // outer time step loop 
  while (time<FinalTime) {

    if (time+dt > FinalTime) {dt=FinalTime-time;}
    tw1=timer.read();   // time NDG work

    // 3rd order SSP Runge-Kutta
    this->RHS(Q, time,BCSolution);  Q1 = Q + dt*rhsQ;
    this->RHS(Q1,time,BCSolution);  Q2 = (3.0*Q + Q1 + dt*rhsQ)/4.0;
    this->RHS(Q2,time,BCSolution);  Q  = (Q + 2.0*Q2 + 2.0*dt*rhsQ)/3.0;

    time += dt;       // increment time 
    SetStepSize();    // compute new timestep

    time_work += timer.read() - tw1;  // accumulate cost of NDG work
    Report();         // optional reporting
    ++tstep;          // increment timestep

  }

  time_total = timer.read()-ti0;  // stop timing
  FinalReport();                  // final report
}

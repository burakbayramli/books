// Euler3D_Run.cpp
// function [Q] = Euler3D(Q, FinalTime, ExactSolution, ExactSolutionBC)
// 2007/07/11
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "Euler3D.h"


void Make3DCouetteGeom ( 
  double r1, int Ntheta1,   // inner cylinder
  double r2, int Ntheta2,   // outer cylinder
  double zh, int Nz         // height and vert. slices
);

//---------------------------------------------------------
void Euler3D::Run()
//---------------------------------------------------------
{
  // function [rho,rhou,rhov,rhow,Ener] = Euler3D(rho,rhou,rhov,rhow,Ener,FinalTime);
  // Purpose  : Integrate 3D Euler equations using a 4th order low storage RK

#if (0)
//Make3DCouetteGeom(1.0, 16,  1.5, 16,  1.0, 1);
  Make3DCouetteGeom(1.0, 42,  1.5, 64,  0.5, 1);
//Make3DCouetteGeom(1.0,  8,  1.5, 16,  1.0, 1);
  return;
#endif

  ti0=timer.read();   // start timing
  InitRun();          // prepare simulation

  int n=0;  DMat Qn("Qn"), rhsQn("rhsQn");

  // Initialize filter?
  if (m_bApplyFilter) 
  { 
  //m_Filter = Filter3D_cutoff(N, 0.95);
  //m_Filter = Filter3D_cutoff(N, 0.90);

  //m_Filter = Filter3D_exp   (0, 16.); 
    m_Filter = Filter3D_exp   (3, 10.); 
  //m_Filter = Filter3D_exp   (4, 4.); 
  //m_Filter = Filter3D_exp   (4, 2.); 
  //m_Filter = Filter3D_exp   (0, 6.); 
  //m_Filter = Filter3D_exp   (0, 4.); 
  //return;
  }

  // filter initial solution?
  if (m_bApplyFilter) { for(n=1;n<=5;++n) {Qn.borrow(Np,K, Q.pCol(n)); Q(All,n)=m_Filter*Qn;}}


  //--------------------------------------------------
  // outer time step loop 
  //--------------------------------------------------
  while (time<FinalTime)
  {
    tw1=timer.read();   // time NDG work

    // adjust final step to end exactly at FinalTime
    if (time+dt > FinalTime) { dt = FinalTime-time; }
    
    for (int INTRK=1; INTRK<=5; ++INTRK) {

      //----------------------------------------------
      // compute RHS of compressible Euler equations
      //----------------------------------------------
      this->RHS(Q, time, BCSolution);

      // filter residual ?
      if (m_bApplyFilter) { for (n=1; n<=5; ++n) { rhsQn.borrow(Np,K, rhsQ.pCol(n)); rhsQ(All,n)=m_Filter*rhsQn; } }

      // initiate and increment Runge-Kutta residuals
      resQ *= rk4a(INTRK); resQ += dt*rhsQ;

      // update fields
      Q += rk4b(INTRK)*resQ;
    }

    time += dt;       // increment current time
    SetStepSize();    // compute new timestep

    time_work += timer.read() - tw1;  // accumulate cost of NDG work
    Report();         // optional reporting
    ++tstep;          // increment timestep

    // if (tstep>=5) break;
  }

  time_total = timer.read()-ti0;  // stop timing
  FinalReport();                  // final report
}

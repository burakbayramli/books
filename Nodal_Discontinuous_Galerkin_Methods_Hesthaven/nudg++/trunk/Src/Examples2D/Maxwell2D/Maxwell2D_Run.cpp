// Maxwell2D.m
// function [Hx,Hy,Ez,time] = Maxwell2D(Hx, Hy, Ez, FinalTime)
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "Maxwell2D.h"

//---------------------------------------------------------
void Maxwell2D::Run()
//---------------------------------------------------------
{
  // function [Hx,Hy,Ez] = Maxwell2D(Hx, Hy, Ez, FinalTime)
  // Purpose  : Integrate TM-mode Maxwell's until FinalTime 
  //            starting with initial conditions Hx,Hy,Ez

  InitRun();          // prepare simulation
  ti0=timer.read();   // start timing

  // outer time step loop 
  while (time<FinalTime) 
  {
    tw1=timer.read();   // time NDG work

    // adjust final step to end exactly at FinalTime
    if (time+dt > FinalTime) { dt = FinalTime-time; }

    for (int INTRK=1; INTRK<=5; ++INTRK) {

      // compute rhs of TM-mode Maxwell's equations
      this->RHS();

      // initiate and increment Runge-Kutta residuals
      resHx *= rk4a(INTRK);   resHx += dt*rhsHx;  
      resHy *= rk4a(INTRK);   resHy += dt*rhsHy; 
      resEz *= rk4a(INTRK);   resEz += dt*rhsEz; 
          
      // update fields
      Hx += rk4b(INTRK)*resHx;  
      Hy += rk4b(INTRK)*resHy;  
      Ez += rk4b(INTRK)*resEz;        
    }

    time_work += timer.read() - tw1;

    time += dt;     // increment current time
    Report();       // optional reporting
    tstep++;        // increment timestep
  }

  time_total = timer.read()-ti0;  // stop timing
  FinalReport();                  // final report
}

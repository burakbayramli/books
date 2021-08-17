// Maxwell3D.m
// function [Hx,Hy,Hz,Ex,Ey,Ez,sampleEz,sampleT,L2errEz] = 
//           Maxwell3D(Hx, Hy, Hz, Ex, Ey, Ez, FinalTime)
// 2007/06/11
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "Maxwell3D.h"

//---------------------------------------------------------
void Maxwell3D::Run()
//---------------------------------------------------------
{
  // function [Hx,Hy,Hz,Ex,Ey,Ez,sampleEz,sampleT] = ...
  //                Maxwell3D(Hx, Hy, Hz, Ex, Ey, Ez, FinalTime)
  //
  // Purpose  : Integrate 3D Maxwell's until FinalTime starting with
  //            initial conditions Hx,Hy,Hz, Ex,Ey,Ez

  ti0=timer.read();   // start timing
  InitRun();          // prepare simulation

#if (0)
  OutputNodes(false);
  OutputNodes(true);
  return;
#endif


  // sample point 
//Sample3D(0.1, 0.2, 0.3, sampleweights, sampletet);
  Sample3D(1.25, 0.0, 0.25, sampleweights, sampletet);


  // outer time step loop 
  while (time<FinalTime)
  {
    tw1=timer.read();   // time NDG work

    // adjust final step to end exactly at FinalTime
    if (time+dt > FinalTime) { dt = FinalTime-time; }

    // inner multi-stage Runge-Kutta loop
    for (int INTRK=1; INTRK<=5; ++INTRK) {
      
      // compute right hand side of TM-mode Maxwell's equations
      //[rhsHx, rhsHy, rhsHz, rhsEx, rhsEy, rhsEz] = 
      this->RHS(); // (Hx,Hy,Hz, Ex, Ey, Ez);
      
      // initiate, increment Runge-Kutta residuals and update fields
      resHx *= rk4a(INTRK);   resHx += dt*rhsHx;
      resHy *= rk4a(INTRK);   resHy += dt*rhsHy;
      resHz *= rk4a(INTRK);   resHz += dt*rhsHz;
      resEx *= rk4a(INTRK);   resEx += dt*rhsEx;
      resEy *= rk4a(INTRK);   resEy += dt*rhsEy;
      resEz *= rk4a(INTRK);   resEz += dt*rhsEz;

      Hx += rk4b(INTRK)*resHx;
      Hy += rk4b(INTRK)*resHy;
      Hz += rk4b(INTRK)*resHz;

      Ex += rk4b(INTRK)*resEx;
      Ey += rk4b(INTRK)*resEy;
      Ez += rk4b(INTRK)*resEz;
    }

    time_work += timer.read() - tw1;

    time += dt;     // increment current time
    Report();       // optional reporting
    tstep++;        // increment timestep

    if (tstep>=10) {break;}
  }

  time_total = timer.read()-ti0;  // stop timing
  FinalReport();                  // final report
}

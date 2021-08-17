// NDG3DDriver.cpp
//
// 2007/07/11
//---------------------------------------------------------
#include "NDGLib_headers.h"

// declare available simulators
#include "Maxwell3D.h"
#include "Euler3D.h"
#include "TestPoissonIPDG3D.h"
#include "ROHOP3D.h"



//---------------------------------------------------------
void NDG3DDriver(int Nsim)
//---------------------------------------------------------
{
//Nsim =  1;   // Maxwell3D
//Nsim =  2;   // Euler3D
  Nsim =  3;   // ROHOP3D
//Nsim = 10;   // Test: PoissonIPDG3D 

  NDG3D* p=NULL;

  try 
  {
    switch (Nsim) {
    case  1: p = new Maxwell3D; break;  // 3D Maxwell simulator
    case  2: p = new Euler3D;   break;  // 3D Euler simulator
    case  3: p = new ROHOP3D;   break;  // 3D ROHOP3D simulator
    // Test drivers -------------------------------------------------
    case 10: p = new TestPoissonIPDG3D;   break;  // PoissonIPDG3D 
    //---------------------------------------------------------------
    default: p = NULL; break;
    }

    if (p) 
    {
      p->Driver();    // call driver
      delete p;       // delete simulator
      umLOG(1, "\nSimulation complete.\n\n");
    } 
    else {
      umWARNING("NDG3DDriver", "No simulator created");
    }
  } catch (...) {umWARNING("NDG3DDriver", "exception caught from sim %d", Nsim);}

}


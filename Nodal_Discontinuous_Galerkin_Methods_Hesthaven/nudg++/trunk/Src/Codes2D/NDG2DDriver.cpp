// NDG2DDriver.cpp
//
// 2007/07/25
//---------------------------------------------------------
#include "NDGLib_headers.h"

// declare available simulators
#include "Maxwell2D.h"
#include "MaxwellCurved2D.h"
#include "MaxwellNonCon2D.h"
#include "ARBCplane2D.h"      // Maxwell ARBC

#include "CurvedEuler2D.h"
#include "CurvedINS2D.h"
#include "CurvedCNS2D.h"

#include "EulerShock2D.h"
//#include "PDEWiz2D.h"
//#include "ShockBubble.h"
//#include "Euler2D.h"


//---------------------------------------------------------
void NDG2DDriver(int Nsim)
//---------------------------------------------------------
{
//Nsim =  1;   // Maxwell2D
//Nsim =  2;   // MaxwellCurved2D
//Nsim =  3;   // MaxwellNonCon2D
  Nsim =  4;   // Maxwell-ARBC:plane2D ***
//Nsim =  5;   // CurvedEuler2D
//Nsim =  6;   // CurvedINS2D
//Nsim =  7;   // CurvedCNS2D
//Nsim =  8;   // EulerShock2D
//Nsim = 11;   // PDEWiz

  NDG2D* p=NULL;

#if (0)
  g_PDE2D =NULL;  // PDEWiz2D
#endif

  try 
  {
    switch (Nsim) {
    case  1: p = new Maxwell2D;         break;  // 2D Maxwell
    case  2: p = new MaxwellCurved2D;   break;  // 2D Maxwell: curved elements
    case  3: p = new MaxwellNonCon2D;   break;  // 2D Maxwell: non-conforming elements
    case  4: p = new ARBCplane2D;       break;  // 2D Maxwell: ARBC (plane2D)
    case  5: p = new CurvedEuler2D;     break;  // 2D Euler
    case  6: p = new CurvedINS2D;       break;  // 2D INS simulator
    case  7: p = new CurvedCNS2D;       break;  // 2D CNS simulator
    case  8: p = new EulerShock2D;      break;  // 2D Euler (WENO limiter)

  //case  x: p = new CurvedCNS2D;       break;  // 2D CNS simulator
  //case  x: p = new ViscousBurgers2D;  break;  // 2D Viscous Burgers eqn
  //case  x: p = new AdvectDiff2D;      break;  // 2D Advection-Diffusion
  //case  x: p = new ShockBubble;       break;  // CLAW ShockBubble

#if (0)
    //#####################################################
    case 11:                    // MODEL_PDE_WIZ   2D PDE Wizard
    //-----------------------------------------------------
      // 1. Construct the simulator object
      g_PDE2D = new PDEWiz2D;
      p = g_PDE2D;

      // 2. Create system of equations, etc.
      //p = (g_PDE2D->Create() ? g_PDE2D : NULL);
      //if (!p) {umWARNING("NDG2DDriver","An error occured in PDEWiz2D::Create()");return;}
      break;
    //#####################################################
#endif


    default: p = NULL; break;
    }

    if (p) 
    {
      p->Driver();    // call driver
      delete p;       // delete simulator
      umLOG(1, "\nSimulation complete.\n\n");
    } 
    else {
      umWARNING("NDG2DDriver", "No simulator created");
    }
  } catch (...) {umWARNING("NDG2DDriver", "exception caught from sim %d", Nsim);}

}


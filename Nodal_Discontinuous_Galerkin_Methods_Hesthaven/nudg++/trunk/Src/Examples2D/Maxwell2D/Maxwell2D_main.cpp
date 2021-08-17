// NDG.cpp: entry point for the NDG (console version)
// Note: reduced version (Maxwell2D only)
// 2007/05/26
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG_headers.h"
#include "Maxwell2D.h"    // Maxwell2D -- TM

//---------------------------------------------------------
int main(int argc, char* argv[])
//---------------------------------------------------------
{
  InitGlobalInfo();     // create global data and open logs

  umLOG(1, "\n");
  umLOG(1, "--------------------------------\n");
  umLOG(1, "              NuDG++            \n");
  umLOG(1, "  Nodal Discontinuous Galerkin  \n");
  umLOG(1, "     Method for non-linear      \n");
  umLOG(1, "          PDE systems           \n");
  umLOG(1, "                                \n");
  umLOG(1, "   o  version 3.0.0             \n");
  umLOG(1, "   o  June 6, 2007              \n");
  umLOG(1, "   o  Dr Tim Warburton          \n");
  umLOG(1, "   o    \n");
  umLOG(1, "--------------------------------\n\n");

  NDG2D *p = new Maxwell2D; // 2D Maxwell simulator
  
  if (p) 
    {
      p->Driver();    // call driver
      delete p;       // delete simulator
      
      umLOG(1, "\nSimulation complete.\n\n");
    } else { 
      umWARNING("NDGDriver", "No simulator created"); 
    }

  FreeGlobalInfo();     // release global data and close logs
  return 0;
}

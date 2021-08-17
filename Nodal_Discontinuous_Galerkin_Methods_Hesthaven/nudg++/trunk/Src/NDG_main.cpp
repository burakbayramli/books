// NDG_main.cpp: entry point for NDG simulations.
//
// 2007/10/18
//---------------------------------------------------------
#include "NDG_headers.h"

void  test_Dense();
int   Test_All_Dense(int seed=1111);
void  NDG2DDriver(int Nsim);
void  NDG3DDriver(int Nsim);


//---------------------------------------------------------
int main(int argc, char* argv[])
//---------------------------------------------------------
{
#ifdef WIN32
  // boost priority
//::SetPriorityClass(GetCurrentProcess(), HIGH_PRIORITY_CLASS);
//::SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_TIME_CRITICAL);
#endif

  // create global data and open logs
  InitGlobalInfo();

  umLOG(1, "\n");
  umLOG(1, "--------------------------------\n");
  umLOG(1, "             NuDG++             \n");
  umLOG(1, "  Nodal Discontinuous Galerkin  \n");
  umLOG(1, "     Method for non-linear      \n");
  umLOG(1, "          PDE systems           \n");
  umLOG(1, "                                \n");
  umLOG(1, "   o  version 3.1.8             \n");
  umLOG(1, "   o  October 18, 2007          \n");
  umLOG(1, "   o  Dr Tim Warburton          \n");
  umLOG(1, "   o  tim.warburton@gmail.com   \n");
  umLOG(1, "--------------------------------\n\n");


  //-------------------------------------
  // TODO: Select simulation from Menu
  //-------------------------------------
  bool run2D=false;

  if (run2D) {
    NDG2DDriver(1);     // 2D simulations
  } else {
    NDG3DDriver(1);     // 3D simulations
  }

  // release global data and close logs
  FreeGlobalInfo();

  return 0;
}

// INIT.cpp
// Global initialization routines.
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"

#if (THIS_IS_READY)
#include "Info_PDEWiz2D.h"  // PDE-Wizard parameters
#endif

bool InitGlobalInfo();
void FreeGlobalInfo();
void InitLogBuffers();
void InitCmdLineArgs();
void InitPDEWizInfo();
void CloseTraceFile();

// flag to manage restarts
static bool bInitial = true;

// Adjust logging output
extern int g_LOG_FLAG;  // [0:10]
extern int g_MSG_FLAG;  // [0:10]
extern int g_TRC_FLAG;  // [0:10]


//---------------------------------------------------------
bool InitGlobalInfo()
//---------------------------------------------------------
{
  // enable tracing for debugging
  // umDVec::set_trace(true);

  // Adjust logging output
#if defined(_DEBUG) || defined(DEBUG)
  g_LOG_FLAG = 1;  // [0:10]  5 -> show most
  g_MSG_FLAG = 4;  // [0:10]  5 -> show most
  g_TRC_FLAG = 5;  // [0:10]  1 -> show crucial
#else
  g_LOG_FLAG = 1;  // [0:10]  5 -> show most
  g_MSG_FLAG = 1;  // [0:10]  5 -> show most
  g_TRC_FLAG = 1;  // [0:10]  1 -> show crucial
#endif


  try {

    // Clear existing data
    FreeGlobalInfo();
    bInitial = false;

    srand((unsigned)time(NULL));        // seed random number system

    //------------------------------------------------
    // Assume: sequence of initialization matters.
    //------------------------------------------------
    InitLogBuffers();     // 1: logging subsystem
    InitCmdLineArgs();    // 2: command line args
#if (THIS_IS_READY)
    InitPDEWizInfo();     // 4: PDE-Wizard parameters
#endif
  }
  catch (...) {
    printf("\n\n>>> Caught exception from InitGlobalInfo() <<<\n\n");
    return false;
  }

  return true;
}


//---------------------------------------------------------
void FreeGlobalInfo()
//---------------------------------------------------------
{
  // Delete global data objects

#if (THIS_IS_READY)

  if (g_Info_PDEWiz) {      // 4: PDE-Wiz info
    delete g_Info_PDEWiz;
    g_Info_PDEWiz = NULL;
  }

#endif


  // g_Info_PDEWiz writes messages during dtor,
  // so delete message buffers AFTER the above!

  if (g_LOGFile) { fclose(g_LOGFile); g_LOGFile = NULL; }
  if (g_MSGFile) { fclose(g_MSGFile); g_MSGFile = NULL; }

  // Expression compiler logs
  if (EC_fout) { fclose(EC_fout); EC_fout = NULL; }
  if (EC_ferr) { fclose(EC_ferr); EC_ferr = NULL; }

  CloseTraceFile();
}


//
// TODO: Use atexit() to register this routine.
//
//
//    bool umGUIApp::OnInit(void)
//    {
//      // Global access
//      theApp = this;
//
//      atexit(CloseTraceFile);
//      ...
//    }
//

///---------------------------------------------------------
void CloseTraceFile()
//---------------------------------------------------------
{

#if (0)
  //-------------------------------------
  // Check state of array registries
  //-------------------------------------
  DVec A(  10, "Sml. RegDump");
  DVec B( 200, "Med. RegDump");
  DVec C(1000, "Big  RegDump");
  A.show_registry();
  B.show_registry();
  C.show_registry();
#endif

  if (0) //g_TRCFile)  
  {
    fclose(g_TRCFile); 
    g_TRCFile = NULL; 
  }
}


//---------------------------------------------------------
void InitCmdLineArgs()
//---------------------------------------------------------
{
  //-------------------------------------
  // Check cl_argc/v have been assigned
  //-------------------------------------
  //assert (cl_argc>0);
  //assert (cl_argv  );
}


//---------------------------------------------------------
void InitPDEWizInfo()
//---------------------------------------------------------
{
#if (THIS_IS_READY)

  // PDE-Wizard parameters
  try {
    g_Info_PDEWiz = new Info_PDEWiz2D;
    assert(g_Info_PDEWiz);

#if (1)
    //###################################
    // NBN: testing demo 2D Euler system
  //g_Info_PDEWiz->LoadEulerTest();
  //g_Info_PDEWiz->LoadFile("Wiz/Euler/Euler_WALL.eqw");
  //g_Info_PDEWiz->LoadFile("Wiz/Euler/Euler_FLOW.eqw");
  //g_Info_PDEWiz->LoadFile("Wiz/Euler/Euler_Mach3.eqw");
  //g_Info_PDEWiz->LoadFile("Wiz/CEM/Maxwell_1.eqw");
  //g_Info_PDEWiz->SaveFile("E_M3_out");
    //###################################
#endif

  } catch(...) {
    umWARNING("InitPDEWizInfo", "error creating g_Info_PDEWiz");
  }

#endif
}


//---------------------------------------------------------
void InitLogBuffers()
//---------------------------------------------------------
{
  g_pStdErr = ::freopen("gp_stderr.txt", "w", stderr);  // reassign stderr
  assert(g_pStdErr);
  fprintf(stderr,     "Printing to stderr\n");
  fprintf(g_pStdErr,  "Printing to g_pStdErr\n");
  fflush (g_pStdErr);

#if (1)
  // for console mode version, send log to console
  fprintf(g_pStdErr,  "Sending LOG output to console\n");
#else
  g_pStdOut = ::freopen("gp_stdout.txt", "w", stdout);  // reassign stdout
  assert(g_pStdOut);
  fprintf(stdout,     "Printing to stdout\n");
  fprintf(g_pStdOut,  "Printing to g_pStdOut\n");
  fflush (g_pStdOut);
#endif

  // Redirect expression compiler messages to file
  EC_fout = ::fopen("EC_Log.txt","w"); assert(EC_fout);
  EC_ferr = ::fopen("EC_Err.txt","w"); assert(EC_ferr);

  // Redirect logs to files in console mode
  g_LOGFile = ::fopen("UMLOG_log.txt","w"); assert(g_LOGFile);
  g_MSGFile = ::fopen("UMLOG_msg.txt","w"); assert(g_MSGFile);
  g_TRCFile = ::fopen("UMLOG_trc.txt","w"); assert(g_TRCFile);
}

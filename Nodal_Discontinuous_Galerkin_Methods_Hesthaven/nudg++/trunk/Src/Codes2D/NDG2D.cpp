// NDG2D.cpp
// 
// 2007/07/03
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"
#include "VecSort_Type.h"

NDG2D* g_D2 = NULL;        // global pointer

int     NDG2D::N2Dobjects=0;      // static counter
int     NDG2D::PlotNumber=0;      // accumulate plot count
double  NDG2D::TotalSimTime=0.0;  // accumulate sim time

//---------------------------------------------------------
NDG2D::NDG2D()
//---------------------------------------------------------
: 
  //-------------------------------------
  // initialize member data
  //-------------------------------------
  ti0(0.0), ti1(0.0)

{
  ++N2Dobjects;       // track allocation of NDG2D objects
  timer.start();      // initialize the timer
  g_D2 = this;        // set global simulation pointer

  // useful totals
  m_Np_K=0;
  m_Nfp_Nfaces_K=0;
  m_Nfp_Nfaces=0;
  m_Np_Nfields=0;

  // reporting parameters
  Nreport     = 10;   // frequency of reporting
  Nrender     = 10;   // frequency of rendering
  Nplotfield  = 1;    // selected field to plot
  NvtkInterp  = 6;    // Vtk interpolation order

  // boolean flags
  m_bMenuLoaded     = false;
  m_bStationary     = false;
  m_bHasAnalyticSol = false;
  m_bArgsSet        = false;
  m_bSummaryShown   = false;
  m_bHeaderShown    = false;
  m_bContinue       = true;
  m_bUserStop       = false;
  m_bDoTest         = false;
  m_bUseAMR         = false;
  m_bAdapted        = false;
  m_bApplyFilter    = false;

  // get machine precision for relative tol. tests
  m_eps = 1e-12;
  double tol1 = 1.0 + m_eps;
  while (tol1 > 1.0) {
    m_eps = 0.5*m_eps;
    tol1 = 1.0 + m_eps;
  }

  // default: no h-refinement of mesh
  Nrefine = 0; refine_count = 0;

  // only valid if this->HasAnalyticSol();
  m_maxAbsError = -9.9e9;

  class_name = "NDG2DSim";
  umTRC(3, "Created NDG2D object (no. %d) \n", N2Dobjects);
}


//---------------------------------------------------------
NDG2D::~NDG2D()
//---------------------------------------------------------
{
  timer.stop();     // finished timing
  --N2Dobjects;     // decrement count of 2D simulators

  if (g_D2 == this) {
    // invalidate global pointer
    g_D2 = NULL;
  }

  umTRC(3, "Deleted NDG2D object (%d remain).\n", N2Dobjects);
}


//---------------------------------------------------------
void NDG2D::InitRun()
//---------------------------------------------------------
{
  // perform any initializations that should 
  // be done between multiple runs:

  time    =  0.0;     // reset time
  RKtime  =  0.0;     // reset RKtime
  tstep   =  1;       // reset current step
//Nsteps  =  0;       // reset number of steps
//Nreport =  2;       // set frequency of reporting (param)
//Nreport =  5;       // set frequency of reporting (param)
  Nreport = 20;       // set frequency of reporting (param)
//Nreport = 1000;     // no reports when timing
  Nrender = Nreport;  // output frequency           (param)

  // reset time counters
  ti0 = ti1   = 0.0;
  time_rhs    = 0.0;
  time_rhs_c  = 0.0;
  time_flux   = 0.0;
  time_upw    = 0.0;
  time_bc     = 0.0;
  time_source = 0.0;
  time_iter   = 0.0;
  time_limit  = 0.0;
  time_work   = 0.0;
  time_total  = 0.0;
}


//---------------------------------------------------------
void NDG2D::Summary()
//---------------------------------------------------------
{
  if (m_bSummaryShown) {
    return;
  }

  //-------------------------------------
  // default summary of current simulator
  //-------------------------------------
  umLOG(1, "\nNuDG++ 2D simulation:\n"
             "  Model type  = %s\n"
             "  Order (N)   = %d\n"
             "  Np          = %d\n"
             "  K           = %d\n"
             "  FileName    = %s\n"
             "  report freq = %d\n"
             "  render freq = %d\n"
             "  plot field  = %d\n"
             "  Vtk interp  = %d\n",
             this->GetClassName(), this->N, this->Np,
             this->K, GetMeshFileName(),
             Nreport, Nrender, Nplotfield, NvtkInterp);

  if (this->Stationary()) {
    umLOG(1, "  Stationary  = true\n\n");
  } else {
    umLOG(1, "  Finaltime   = %0.2g\n"
             "  time-step   = %0.5g  (inital dt)\n"
             "  num. steps  = %d\n\n\n", 
             this->FinalTime, this->dt, this->Nsteps);
  }

  m_bSummaryShown = true;
}


//---------------------------------------------------------
void NDG2D::Report(bool bForce)
//---------------------------------------------------------
{}

//---------------------------------------------------------
void NDG2D::FinalReport()
//---------------------------------------------------------
{}


//---------------------------------------------------------
double NDG2D::GetAnalyticError()
//---------------------------------------------------------
{
  // only valid if this->HasAnalyticSol();
  return m_maxAbsError;
}


//---------------------------------------------------------
void NDG2D::AdjustCylBC
(
  double radius,  // radius
  double Cx,      // center x
  double Cy,      // center y
  int    bc,      // select boundary faces
  bool   toWall   // convert faces to BC_Wall?

)
//---------------------------------------------------------
{
  // Adjust elements with faces on circular boundaries so that 
  // their nodes lie precisely on the circle of given radius.
  //
  // Note that this routine accommodates multiple circular 
  // boundaries, but that each distinct circle must have a 
  // distinct boundary condition.  Sets of curved elements 
  // are extracted in temp vector "curvedBC" and accumulated 
  // in member array "curved".

  // store original BC data (assumes 1 call to "AdjustCylBC")
  // TODO: adjust for multiple circular boundaries (e.g. Couette)
  saveBCType = BCType;

  IMat cylfaces; 
  if (BC_All == bc) {
    // faces marked with any boundary condition
    cylfaces = find2D(BCType, '>', (int)BC_None);
  } else {
    // faces marked with selected bc
    cylfaces = find2D(BCType, '=', bc);
  }
  if (cylfaces.num_rows()>0)
  {
    //#####################################################
    // FIXME: accommodate multiple circular boundaries
    // curved.append(curvedBC)
    // straight.append(straightBC)
    //#####################################################

    IVec k=cylfaces(All,1);       // elem indices
  //IVec f=cylfaces(All,2);       // face indices
    IVec curvedBC = sort(k,true); // curved elements on this boundary

    MakeCylinder2D(cylfaces, radius, Cx, Cy);
    if (toWall) {
      // turn cylinders into walls
      IVec ids = find(BCType, '=', (int)BC_Cyl);
      BCType(ids) = BC_Wall;
    }

    // accumlate sets of curved boundaries
    curved.append(curvedBC);
    if (curved.size() > curvedBC.size()) {
      curved = sort(curved,true);
    }

    straight = setdiff( Range(1,K), curved );
  } 
  
  else 
  {
    if (curved.size() > 0) 
    {
      umERROR("NDG2D::AdjustCylBC", "");
      straight = setdiff( Range(1,K), curved );
    }
    straight.range(1,K); 
    curved.resize(0);
  }

}


//---------------------------------------------------------
void NDG2D::CalcElemCentroids(DMat& centroid)
//---------------------------------------------------------
{
  // Calculate element centroids for volume, face nodes
  centroid.resize (K, 2);
  double x1,x2,x3, y1,y2,y3;
  for (int k=1; k<=K; ++k) {
    x1 = this->VX(EToV(k,1)); x2 = this->VX(EToV(k,2)); x3 = this->VX(EToV(k,3));
    y1 = this->VY(EToV(k,1)); y2 = this->VY(EToV(k,2)); y3 = this->VY(EToV(k,3));
    // assign a centroid for element k
    centroid(k,1) = (x1+x2+x3)/3.0;
    centroid(k,2) = (y1+y2+y3)/3.0;
  }
}

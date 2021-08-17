// NDG3D.cpp
// 
// 2007/07/11
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG3D.h"


NDG3D* g_D3 = NULL;        // global pointer

int     NDG3D::N3Dobjects=0;      // static counter
int     NDG3D::PlotNumber=0;      // accumulate plot count
double  NDG3D::TotalSimTime=0.0;  // accumulate sim time

//---------------------------------------------------------
NDG3D::NDG3D()
//---------------------------------------------------------
: 
  //-------------------------------------
  // initialize member data
  //-------------------------------------
  ti0(0.0), ti1(0.0)

{
  ++N3Dobjects;       // track allocation of NDG3D objects
  timer.start();      // initialize the timer
  g_D3 = this;        // set global simulation pointer

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

  class_name = "NDG3DSim";
  umTRC(3, "Created NDG3D object (no. %d) \n", N3Dobjects);
}


//---------------------------------------------------------
NDG3D::~NDG3D()
//---------------------------------------------------------
{
  timer.stop();     // finished timing
  --N3Dobjects;     // decrement count of 2D simulators

  if (g_D3 == this) {
    // invalidate global pointer
    g_D3 = NULL;
  }

  umTRC(3, "Deleted NDG3D object (%d remain).\n", N3Dobjects);
}


//---------------------------------------------------------
void NDG3D::InitRun()
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
void NDG3D::Summary()
//---------------------------------------------------------
{
  //-------------------------------------
  // default summary of current simulator
  //-------------------------------------
  umLOG(1, "\nNuDG++ 3D simulation:\n"
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
void NDG3D::Report(bool bForce)
//---------------------------------------------------------
{}

//---------------------------------------------------------
void NDG3D::FinalReport()
//---------------------------------------------------------
{}


//---------------------------------------------------------
double NDG3D::GetAnalyticError()
//---------------------------------------------------------
{
  // only valid if this->HasAnalyticSol();
  return m_maxAbsError;
}


#if (0)
//#########################################################
void NDG3D::AdjustSphBC
(
  double radius,  // radius
  double Cx,      // center x
  double Cy,      // center y
  double Cz,      // center z
  int    bc       // select boundary faces
)
//#########################################################
{
  // Store original BC data
  saveBCType = BCType;

  IMat sphfaces; 
  if (BC_All == bc) {
    // faces marked with any boundary condition
    sphfaces = find2D(BCType, '>', (int)BC_None);
  } else {
    // faces marked with selected bc
    sphfaces = find2D(BCType, '=', bc);
  }
  if (sphfaces.num_rows()>0)
  {
    IVec k=sphfaces(All,1);    // elem indices
  //IVec f=sphfaces(All,2);    // face indices
    curved = sort(k,true);
    straight = setdiff( Range(1,K), curved );
    MakeSphere3D(sphfaces, radius, Cx, Cy, Cz);
    // turn cylinders into walls
    //IVec ids = find(BCType, '=', (int)BC_Cyl);
    //BCType(ids) = BC_Wall;
  } else {
    straight.range(1,K); 
    curved.resize(0);
  }

}
//#########################################################
#endif


//---------------------------------------------------------
void NDG3D::CalcElemCentroids(DMat& centroid)
//---------------------------------------------------------
{
  // Calculate element centroids for volume, face nodes
  centroid.resize (K, 3);
  double x1,x2,x3,x4, y1,y2,y3,y4, z1,z2,z3,z4;
  for (int k=1; k<=K; ++k) {
    x1 = this->VX(EToV(k,1)); x2 = this->VX(EToV(k,2)); x3 = this->VX(EToV(k,3)); x4 = this->VX(EToV(k,4));
    y1 = this->VY(EToV(k,1)); y2 = this->VY(EToV(k,2)); y3 = this->VY(EToV(k,3)); y4 = this->VY(EToV(k,4));
    z1 = this->VZ(EToV(k,1)); z2 = this->VZ(EToV(k,2)); z3 = this->VZ(EToV(k,3)); z4 = this->VZ(EToV(k,4));
    // assign a centroid for element k
    centroid(k,1) = (x1+x2+x3+x4)/4.0;
    centroid(k,2) = (y1+y2+y3+y4)/4.0;
    centroid(k,3) = (z1+z2+z3+z4)/4.0;
  }
}


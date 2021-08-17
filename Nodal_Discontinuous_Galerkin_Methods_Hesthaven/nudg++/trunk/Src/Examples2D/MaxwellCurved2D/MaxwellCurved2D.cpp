// MaxwellCurved2D.cpp
// member routines for class MaxwellCurved2D
// 2007/06/10
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "MaxwellCurved2D.h"


//---------------------------------------------------------
MaxwellCurved2D::MaxwellCurved2D()
//---------------------------------------------------------
{
  class_name = "MaxwellCurved2D-TM";
}


//---------------------------------------------------------
MaxwellCurved2D::~MaxwellCurved2D()
//---------------------------------------------------------
{
}


//---------------------------------------------------------
void MaxwellCurved2D::SetIC()
//---------------------------------------------------------
{
#if (0)
  // NBN: to compare with base version
  // Maxwell2D::SetIC();
  // return;
#endif
  
  // Set initial conditions for simulation

  // First 6 modes of eigenmodes with 6 azimuthal periods
  m_alpha.resize(6);
  m_alpha(1) =  9.936109524217684;
  m_alpha(2) = 13.589290170541217;
  m_alpha(3) = 17.003819667816014;
  m_alpha(4) = 20.320789213566506;
  m_alpha(5) = 23.586084435581391;
  m_alpha(6) = 26.820151983411403;

  // this configuration has an analytic solution
  // Note: analytic sol. depends on m_Ezinit:
  m_bHasAnalyticSol = true;

  // choose radial mode
  alpha0 = m_alpha(2);
  m_theta  = atan2(y,x);
  m_rad    = sqrt(sqr(x) + sqr(y));

//Ez = besselj(6, alpha0*rad).*cos(6*theta);
  DVec tbsslj = besselj(6, alpha0 * m_rad);
  DVec tcosth = apply(cos, (6.0 * m_theta));
  Ezinit = tbsslj.dm(tcosth);

  Ez = Ezinit;
  Hx = 0.0;
  Hy = 0.0;
}


//---------------------------------------------------------
void MaxwellCurved2D::InitRun()
//---------------------------------------------------------
{
  StartUp2D();        // construct grid and metric

#if (0)
  umLOG(1, "before refine : K = %5d\n", K);
  //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  DMat Q2(Np*K, 1);  IMat refineflag;
  refineflag = Ones(K,Nfaces); Q2 = ConformingHrefine2D(refineflag, Q2);
  umLOG(1, "after refine 1: K = %5d\n", K);
  //refineflag = Ones(K,Nfaces); Q2 = ConformingHrefine2D(refineflag, Q2);
  //umLOG(1, "after refine 1: K = %5d\n", K);
  //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#endif


  BuildBCMaps2D();    // build boundary condition maps
//OutputNodes(true);  // face nodes
  AdjustCylBC(1., 0.,0., BC_All); // Push all boundary faces to unit cylinder
//OutputNodes(true);  // face nodes
  Resize();           // allocate work arrays
  SetIC();            // set initial conditions
  SetStepSize();      // calculate step size (dt)

  BuildCurvedOPS2D(3*N);

  //---------------------------------------------
  // base class version sets counters and flags
  //---------------------------------------------
  NDG2D::InitRun();   

  //---------------------------------------------
  // Adjust reporting and render frequencies
  //---------------------------------------------
  Nreport =  Nsteps/20;
//Nreport =  2;         // set frequency of reporting (param)
//Nreport = 10;         // set frequency of reporting (param)
//Nreport = 50;        // set frequency of reporting (param)
  Nrender = Nreport;    // output frequency           (param)
//Nrender = 10;         // output frequency           (param)
//Nrender = 100000;     // output frequency           (param)
  
  NvtkInterp = 12;      // set output resolution
//NvtkInterp =  6;      // set output resolution

  Summary();          // Show simulation details
}


//---------------------------------------------------------
void MaxwellCurved2D::FinalReport()
//---------------------------------------------------------
{
  // force report on final step
  this->Report(true);

  // Report error and work info
  if (HasAnalyticSol()) {
    umLOG(1, "\n Max analytic error : %12.4e\n", this->GetAnalyticError());
    umLOG(1,   "----------------------------------\n"); 
  }

  umLOG(1, "\n  time for NDG work:  %0.2lf secs\n",  time_work);
  umLOG(1,   "   rhs work - base :  %0.2lf\n",       time_rhs);
  umLOG(1,   "            - curve:  %0.2lf\n",       time_rhs_c);
  umLOG(1,   "            - total:  %0.2lf secs\n",  time_rhs+time_rhs_c);
  umLOG(1,   " time for main loop:  %0.2lf secs\n\n",time_total);
}


//---------------------------------------------------------
double MaxwellCurved2D::GetAnalyticError()
//---------------------------------------------------------
{
  double tfac = time * cos(alpha0*time);
  m_ErrAnalytic = Ez - (tfac*Ezinit);
  m_maxAbsError = m_ErrAnalytic.max_val_abs();
  return m_maxAbsError;
}

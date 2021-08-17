// CurvedEuler2D.cpp
// member routines for class CurvedEuler2D
// 2007/06/21
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedEuler2D.h"


//---------------------------------------------------------
CurvedEuler2D::CurvedEuler2D()
//---------------------------------------------------------
{
  class_name = "CurvedEuler2D";

  // set simulation parameters
  gamma = 1.4;
  gm1   = gamma - 1.0;
}


//---------------------------------------------------------
CurvedEuler2D::~CurvedEuler2D()
//---------------------------------------------------------
{
}


//---------------------------------------------------------
void CurvedEuler2D::Resize()
//---------------------------------------------------------
{
  // Allocate member arrays
  Q.resize   (Np*K,  4);    // solution fields
  rhsQ.resize(Np*K,  4);    // Runge-Kutta stage values
  resQ.resize(Np*K,  4);    // Runge-Kutta residual 
}


//---------------------------------------------------------
void CurvedEuler2D::Resize_cub()
//---------------------------------------------------------
{
  // resize cubature arrays
  int Nc = m_cub.Ncub, Ng=m_gauss.NGauss; int Ngf=Ng*Nfaces*K;
  // assumes cub and gauss are ready
  assert(Nc>0 && Ng>0 && Ngf>0);
  cQ.resize(Nc*K,4); cF.resize(Nc*K,4);  cG.resize(Nc*K,4);
  gQ.resize(Ngf,4);  gQM.resize(Ngf,4);  gQP.resize(Ngf,4); 
  flux.resize(Ngf,4);
}


//---------------------------------------------------------
void CurvedEuler2D::MapGaussFaceData()
//---------------------------------------------------------
{
  // override nodal face information with 
  // Gauss nodal face information

  this->nx   = m_gauss.nx;    this->ny   = m_gauss.ny; 
  this->mapW = m_gauss.mapW;  this->mapI = m_gauss.mapI;
  this->mapO = m_gauss.mapO;  this->mapB = m_gauss.mapB;
  this->mapC = m_gauss.mapC;//this->mapS = m_gauss.mapS;
}



//---------------------------------------------------------
void CurvedEuler2D::PreCalcBdryData()
//---------------------------------------------------------
{

  //---------------------------------------------
  // pre-calculated data for IsentropicVortex
  //---------------------------------------------
  if (eIsentropicVortex == sim_type) {
    gmapB = concat(mapI, mapO, mapW);
    gxB = m_gauss.x(gmapB);
    gyB = m_gauss.y(gmapB);
  }

  //---------------------------------------------
  // pre-calculated data for ChannelFlow
  //---------------------------------------------
  else if (eChannelFlow == sim_type) {
    gmapB = m_gauss.mapB;
    gxB = m_gauss.x(m_gauss.mapB);
    gyB = m_gauss.y(m_gauss.mapB);
  }

  //---------------------------------------------
  // pre-calculated data for CouetteFlow
  //---------------------------------------------
  else if (eCouetteFlow == sim_type)  {
    gmapB = concat(mapI, mapO);
    gxB = m_gauss.x(gmapB);
    gyB = m_gauss.y(gmapB);

    DVec   rad2B = sqr(gxB) + sqr(gyB);
    DVec    radB = sqrt(rad2B);
    DVec  thetaB = atan2(gyB, gxB);
    DVec uthetaB = (-radB + 16.0/radB)/75.0;
    DVec      pB = 1.0 + (1.0/SQ(75.0)) * (rad2B/2.0 - 32.0*log(radB) - 128.0/rad2B);

    // store constant boundary data
    rhoB  = ones(gmapB.size());
    rhouB = (-sin(thetaB)).dm(uthetaB);
    rhovB = ( cos(thetaB)).dm(uthetaB);
    EnerB = pB/gm1 + 0.5*(sqr(rhouB)+sqr(rhovB)).dd(rhoB);
  }

  //...

}


//---------------------------------------------------------
void CurvedEuler2D::SetIC()
//---------------------------------------------------------
{
  // compute initial condition (time=0)
  (this->*InitialSolution)(x, y, 0.0, Q);
}


//---------------------------------------------------------
void CurvedEuler2D::SetStepSize()
//---------------------------------------------------------
{
  // function dt = Euler2Ddt(Q, gamma)
  // compute time step for the compressible Euler equations
  DVec rho,rhou,rhov,Ener, u,v,p,c,squv,Fscale_2,w_speeds, q1,q2,q3,q4;

  // since "self-mapping" of arrays is illegal, 
  // e.g. rho = rho(vmapM), use temp wrappers
  q1.borrow(Np*K,Q.pCol(1));  q2.borrow(Np*K,Q.pCol(2));
  q3.borrow(Np*K,Q.pCol(3));  q4.borrow(Np*K,Q.pCol(4));

  rho=q1(vmapM); rhou=q2(vmapM); rhov=q3(vmapM); Ener=q4(vmapM);
  u = rhou.dd(rho); v = rhov.dd(rho);  squv=sqr(u)+sqr(v);

  p = gm1 * (Ener - rho.dm(squv)/2.0);
  c = sqrt(abs(gamma*p.dd(rho)));

  Fscale_2 = 0.5*Fscale;
  w_speeds=SQ(N+1)*Fscale_2.dm(sqrt(squv)+c);
  dt = 1.0/w_speeds.max_val();

  Nsteps = (int)ceil(FinalTime/dt);
  dt = FinalTime/(double)Nsteps;
}


//---------------------------------------------------------
void CurvedEuler2D::InitRun()
//---------------------------------------------------------
{
  StartUp2D();      // construct grid and metric

  //-------------------------------------
  // Adjust faces on circular boundaries
  //-------------------------------------
  switch (sim_type) {

  case eIsentropicVortex:
    // no cylinder faces
    straight.range(1,K); curved.resize(0);
    break;

  case eChannelFlow:
    // no cylinder faces
    straight.range(1,K); curved.resize(0);
    break;

  case eCouetteFlow:
    // move inflow faces to r=4.0       
    AdjustCylBC(4.0, 0.0, 0.0, BC_In);    // outer cylinder
    // move outflow faces to r=1.0
    AdjustCylBC(1.0, 0.0, 0.0, BC_Out);   // inner cylinder
    break;
  
  default:
    // set default maps for {straight,curved} elements
    straight.range(1,K); curved.resize(0);
    break;
  }


  BuildBCMaps2D();  // map faces subject to boundary conditions
  Resize();         // allocate arrays
  SetIC();          // set initial conditions
  SetStepSize();    // compute initial timestep (using IC's)

  // storage for residual at each time-step
  resid.resize(Nsteps+1);

  //---------------------------------------------
  // base class version sets counters and flags
  //---------------------------------------------
  NDG2D::InitRun();   


//Nreport =   1;      // set frequency of reporting
//Nreport =   5;      // set frequency of reporting
//Nreport =  20;      // set frequency of reporting
  Nreport =  50;      // set frequency of reporting
//Nreport = 100;      // no reports when timing
//Nreport = 500;      // no reports when timing

  Nrender = Nreport;  // output frequency
//Nrender = 100;      // output frequency
//Nrender = 10000;    // output frequency

//NvtkInterp = 16;  // set output resolution
//NvtkInterp = 10;  // set output resolution
  NvtkInterp =  8;  // set output resolution
//NvtkInterp =  6;  // set output resolution


  Summary();          // Show simulation details
}


//---------------------------------------------------------
void CurvedEuler2D::Summary()
//---------------------------------------------------------
{
  // TODO: add details of operators and sparse solvers
  NDG2D::Summary();
}


//---------------------------------------------------------
void CurvedEuler2D::Report(bool bForce)
//---------------------------------------------------------
{
  if (1 == tstep) {
    // print header
    umLOG(1, "\n step   time    rho(min) rho(max)   En(min)  En(max)       dt   \n"
               "----------------------------------------------------------------\n");
  }

  if (!umMOD(tstep,Nreport) || bForce || (1==tstep)) 
  {
    double r_min=Q.min_col_val(1), r_max=Q.max_col_val(1);
    double e_min=Q.min_col_val(4), e_max=Q.max_col_val(4);

    umLOG(1, "%5d  %6.3lf   %8.5lf %8.5lf   %8.5lf %8.5lf   %8.6lf\n", 
              tstep, time, r_min, r_max, e_min, e_max, dt);
  }

  if (!umMOD(tstep,Nrender) || bForce)
  {

#if (0)

    Q_plot = Q;

    // extract conserved variables, calculate 
    // vorticity, and load this as 4th field:
    DMat rho, rhou, rhov, u,v, curl;
    rho.borrow(Np,K,Q.pCol(1)); rhou.borrow(Np,K,Q.pCol(2)); rhov.borrow(Np,K,Q.pCol(3));
    u = rhou.dd(rho); v = rhov.dd(rho);
    Curl2D(u, v, curl);
    Q_plot(All,4) = curl;  
    OutputVTK(Q_plot, NvtkInterp);

#else

    OutputVTK(Q, NvtkInterp);

#endif
  }
}


//---------------------------------------------------------
void CurvedEuler2D::FinalReport()
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
  umLOG(1,   "           rhs work:  %0.2lf\n",       time_rhs);
  umLOG(1,   " time for main loop:  %0.2lf secs\n\n",time_total);
}

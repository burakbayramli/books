// CurvedCNS2D.cpp
// member routines for class CurvedCNS2D
// 2007/07/12
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedCNS2D.h"


//---------------------------------------------------------
CurvedCNS2D::CurvedCNS2D() 
//---------------------------------------------------------
{
  class_name = "CurvedCNS2D";

  //-------------------------------------
  // set default parameters
  //-------------------------------------
  gamma = 1.4;      // Gas constant
  gm1   = 0.4;      // (gamma-1)

  mu    = 1e-2;     // 
  pbar  = 10.0;
  pref  = 12.0;
}


//---------------------------------------------------------
CurvedCNS2D::~CurvedCNS2D()
//---------------------------------------------------------
{
}


//---------------------------------------------------------
void CurvedCNS2D::Resize()
//---------------------------------------------------------
{
  // set default maps for {straight,curved} elements
  straight.range(1,K); curved.resize(0);


  // Allocate member arrays
  Q.resize   (Np*K,  4);    // solution fields
  rhsQ.resize(Np*K,  4);    // Runge-Kutta stage values
  resQ.resize(Np*K,  4);    // Runge-Kutta residual 
}


//---------------------------------------------------------
void CurvedCNS2D::Resize_cub()
//---------------------------------------------------------
{
}


//---------------------------------------------------------
void CurvedCNS2D::MapGaussFaceData()
//---------------------------------------------------------
{
}


//---------------------------------------------------------
void CurvedCNS2D::PreCalcBdryData()
//---------------------------------------------------------
{
  gmapB = m_gauss.mapB; 
  gxB = m_gauss.x(gmapB);
  gyB = m_gauss.y(gmapB);

  //---------------------------------------------
  // pre-calculate bdry conds. for CouetteFlow
  //---------------------------------------------
  if (eCouetteFlow == sim_type)  {
    gmapB = concat(m_gauss.mapI, m_gauss.mapO);
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

  //---------------------------------------------
  // pre-calculate data for other simulations
  //---------------------------------------------
  // else if (eCylinderFlow == sim_type) { }
  // else if (eChannelFlow == sim_type) { }
  // else if (eBoxFlow == sim_type) { }

}


//---------------------------------------------------------
void CurvedCNS2D::SetIC()
//---------------------------------------------------------
{
  // compute initial condition (time=0)
  // Q = feval(InitialSolution, x, y, 0.0);
  (this->*InitialSolution)(     x, y, 0.0, Q);
}


//---------------------------------------------------------
void CurvedCNS2D::SetStepSize()
//---------------------------------------------------------
{
  // compute stable time step size for 
  // compressible Navier-Stokes solver
  // dt = 0.25*CurvedCNSdt2D(Q, gamma, mu);

  DVec rho,rhou,rhov,Ener, u,v,p,c,h,lam, q1,q2,q3,q4,rsqr,invsp;
  double N1p2 = double(SQ(N+1));  // (N+1)^2
  double N1p4 = N1p2*N1p2;        // (N+1)^4

  // extract field variables
  // since "self-mapping" of arrays is illegal, 
  // e.g. rho = rho(vmapM), use temp wrappers
  q1.borrow(Np*K,Q.pCol(1));  q2.borrow(Np*K,Q.pCol(2));
  q3.borrow(Np*K,Q.pCol(3));  q4.borrow(Np*K,Q.pCol(4));

  // evaluate fields at surface nodes
  rho=q1(vmapM); rhou=q2(vmapM); rhov=q3(vmapM); Ener=q4(vmapM);

  // compute primitive variables
  u = rhou.dd(rho); v = rhov.dd(rho);  rsqr = sqr(u)+sqr(v);
  p = gm1 * (Ener - rho.dm(rsqr)/2.0);
  c = sqrt(abs(gamma*p.dd(rho)));

  h = 2.0/Fscale;
  lam = sqrt(rsqr) + c; 
  invsp = 1.0 / ( N1p2*lam.dd(h) + (N1p4*mu)/sqr(h) );
  dt = 0.5 * invsp.min_val();

  if (eBoxFlow!=sim_type) {
    dt *= 1.00;   // keep full stepsize
  //dt *= 0.50;   // reduce default stepsize
  //dt *= 0.25;   // reduce default stepsize
  } else {
    dt *= 0.25;   // reduce default stepsize
  }

  Nsteps = (int)ceil(FinalTime/dt);
  dt = FinalTime/(double)Nsteps;
}


//---------------------------------------------------------
void CurvedCNS2D::InitRun()
//---------------------------------------------------------
{
  // construct grid and metric
  StartUp2D();

  // Optional mesh refinement: split each parent 
  // element into 4 conforming "child" elements

  if (Nrefine>0) {
    umLOG(1, "before refine : K = %5d\n", K);
    DMat Q2(Np*K, 1);  IMat refineflag;
    refineflag = Ones(K,Nfaces);
    for (int i=1; i<=Nrefine; ++i) {
      Q2 = ConformingHrefine2D(refineflag, Q2);
      umLOG(1, "after refine %d: K = %5d\n", i, K);
    }
  }

  // Adjust faces on circular boundaries,
  // and perform any sim-specific setup:

  switch (sim_type) {

  case eCylinderFlow:
    // move Cylinder bdry faces to radius 1.0
  //AdjustCylBC(1.0, 0.0, 0.0, BC_Cyl); // test mesh 1: r=1.0
    AdjustCylBC(0.05, 0.,0., BC_Cyl);   // Volker John: move Cyl faces to r=0.05
    break;

  case eBoxFlow:
    // no cylinder faces
    straight.range(1,K); curved.resize(0);
    // but set up periodic maps
    BuildPeriodicMaps2D(1.0, 1.0);
    break;

  case eChannelFlow:
    // no cylinder faces
    straight.range(1,K); curved.resize(0);
    break;

  case eCouetteFlow:

#if (0)
    // move inflow faces to r=4.0       
    AdjustCylBC(4.0, 0.0, 0.0, BC_In);    // outer cylinder
    // move outflow faces to r=1.0
    AdjustCylBC(1.0, 0.0, 0.0, BC_Out);   // inner cylinder
#else
    // move inflow faces to r=4.0       
    AdjustCylBC(2.0, 0.0, 0.0, BC_Out);   // outer cylinder
    // move outflow faces to r=1.0
    AdjustCylBC(1.0, 0.0, 0.0, BC_In);    // inner cylinder
#endif
    break;
  }


  BuildBCMaps2D();      // build boundary condition maps
  Resize();             // allocate arrays
  SetIC();              // set initial conditions
  SetStepSize();        // calculate step size (dt)


  //---------------------------------------------
  // base class version sets counters and flags
  //---------------------------------------------
  NDG2D::InitRun();   

  //---------------------------------------------
  // set frequency of reporting
  //---------------------------------------------
//Nreport =  Nsteps/50;
  Nreport =  1;
//Nreport =  5;
//Nreport = 10;
//Nreport = 50;
  Nreport = 100;
//Nreport = 1000;
//Nreport = 10000;

  //---------------------------------------------
  // set frequency of rendering
  //---------------------------------------------
//Nrender = Nreport;
  Nrender = 5*Nreport;
//Nrender = 100;
//Nrender = 10000;

  NvtkInterp = 6;  // set output resolution

  Summary();  // show simulation details
}


//---------------------------------------------------------
void CurvedCNS2D::Summary()
//---------------------------------------------------------
{
  // TODO: add details of sim_type, flux_type, operators
  NDG2D::Summary();
}


//---------------------------------------------------------
void CurvedCNS2D::Report(bool bForce)
//---------------------------------------------------------
{
  if (1 == tstep) {
    // print header
    umLOG(1, "\n  step    time    rho(min) rho(max)    En(min)  En(max)       dt   \n"
               "------------------------------------------------------------------\n");
  }

  if (!umMOD(tstep,Nreport) || bForce || (1==tstep)) 
  {
    double r_min=Q.min_col_val(1), r_max=Q.max_col_val(1);
    double e_min=Q.min_col_val(4), e_max=Q.max_col_val(4);

    umLOG(1, "%8d  %6.3lf   %8.5lf %8.5lf   %8.5lf %8.5lf  %11.3e\n", 
              tstep, time, r_min, r_max, e_min, e_max, dt);
  }

  if (!umMOD(tstep,Nrender) || bForce)
  {

#if (1)

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
void CurvedCNS2D::FinalReport()
//---------------------------------------------------------
{
  // force report on final step
  this->Report(true);

  // Report error and work info
  // if (HasAnalyticSol()) {
  //   umLOG(1, "\n Max analytic error : %12.4e\n", this->GetAnalyticError());
  //   umLOG(1,   "----------------------------------\n"); 
  // }

  umLOG(1, "\n  time for NDG work:  %0.2lf secs\n",  time_work);
  umLOG(1,   "           rhs work:  %0.2lf\n",       time_rhs);
  umLOG(1,   " time for main loop:  %0.2lf secs\n\n",time_total);
}

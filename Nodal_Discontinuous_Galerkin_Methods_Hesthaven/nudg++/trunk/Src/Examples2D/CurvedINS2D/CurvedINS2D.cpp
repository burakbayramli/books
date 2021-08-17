// CurvedINS2D.cpp
// member routines for class CurvedINS2D
// 2007/06/10
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedINS2D.h"


//---------------------------------------------------------
CurvedINS2D::CurvedINS2D() 
//---------------------------------------------------------
{
  class_name = "CurvedINS2D";

  // Set system parameters and constants
  sim_type = eNone;
  nu = 1.0/40.0; 

  // dual splitting scheme coefficients
//g0= 1.5; a0= 2.0; a1= -0.5; b0= 2.0; b1= -1.0;  // high order
  g0= 1.0; a0= 1.0; a1=  0.0; b0= 1.0; b1=  0.0;  // init order

  // clear Cholesky solvers
  create_solvers();
}


//---------------------------------------------------------
CurvedINS2D::~CurvedINS2D()
//---------------------------------------------------------
{
  // clear Cholesky solvers
  free_solvers();
}


//---------------------------------------------------------
void CurvedINS2D::Resize()
//---------------------------------------------------------
{
  // Allocate storage for member arrays
  Ux.resize(Np, K);  Uy.resize(Np, K);  PR.resize(Np, K);
  bcUx.resize(Np*K); bcUy.resize(Np*K); bcPR.resize(Np*K);
  bcdUndt.resize(Np*K);

  // storage for output
  Q_plot.resize(Np*K, 4);
//Q_plot.resize(Np*K, 3);

  // storage for history of fields and nonlinear terms
  Uxold.resize(Np,K);   NUx.resize(Np,K);
  Uyold.resize(Np,K);   NUy.resize(Np,K);

  // storage for pressure Neumann boundary data
  dpdn.resize(Nfp*Nfaces,K); 
}


//---------------------------------------------------------
void CurvedINS2D::PreCalcBdryData()
//---------------------------------------------------------
{
  //gmapB = m_gauss.mapB; 
  //gxB = m_gauss.x(gmapB);
  //gyB = m_gauss.y(gmapB);

  if (eBackdrop == sim_type) {
    nbcmapD.resize(0);
    vbcmapD.resize(0);
  } else {
    nbcmapD = concat( mapI,  mapW,  mapC);
    vbcmapD = concat(vmapI, vmapW, vmapC);
  } 

  //---------------------------------------------
  // pre-calculate data for other simulations
  //---------------------------------------------
  // if      (eChannel        == sim_type) { }
  // else if (eKovasznay      == sim_type) { }
  // else if (eStep           == sim_type) { }
  // else if (eVolkerCylinder == sim_type) { }
  // else if (ePearsonVortex  == sim_type) { }
  // else if (eChannel        == sim_type) { }
  // else if (eStirer         == sim_type) { }

}


//---------------------------------------------------------
void CurvedINS2D::SetIC()
//---------------------------------------------------------
{
  // evaluate initial data
  //feval(ExactSolution, x, y, 0.0, nu, Ux,Uy,PR);
  (this->*ExactSolution)(x, y, 0.0, nu, Ux,Uy,PR);
}


//---------------------------------------------------------
void CurvedINS2D::SetStepSize()
//---------------------------------------------------------
{
  //-------------------------------------
  // time step parameter
  //-------------------------------------
  dtscale2D(dtscale);
  dt = dtscale.min_val()/ double (SQ(N+1));

  // Note: constraints on penalty methods
  // if(N>1), dt = 2*dt; end;
  // if(N>11) dt = .75*dt; end;
  // if(strmatch(simtype,'PearsonVortex')) dt = .1*dt, end

  // Adjust stepsize for selected sim_type
  // dt = 2.0*dt;    // works for Volker cylinder
  // dt = 0.1*dt;    // ...

  if (ePearsonVortex == sim_type) {
    // reduce stepsize for the vortex test case
  //dt /= 10.0;
    dt /=  5.0;
  }

  Nsteps = (int)ceil(FinalTime/dt);
  dt = FinalTime/(double)Nsteps;
}


//---------------------------------------------------------
void CurvedINS2D::InitRun()
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

  case eVolkerCylinder:
    // move Cylinder bdry faces to radius 0.05
    AdjustCylBC(0.05, 0.,0., BC_Cyl);
    break;

  default:
    // set default maps for {straight,curved} elements
    straight.range(1,K); curved.resize(0);
    break;
  }

  Resize();             // allocate arrays
  BuildBCMaps2D();      // build boundary condition maps
  SetIC();              // set initial conditions
  SetStepSize();        // calculate step size (dt)

  // reset various work timers
  time_setup = time_advection = 0.0;
  time_viscous = time_viscous_sol = 0.0;
  time_pressure = time_pressure_sol = 0.0;

  //---------------------------------------------
  // base class version sets counters and flags
  //---------------------------------------------
  NDG2D::InitRun();   

  //---------------------------------------------
  // set frequency of reporting
  //---------------------------------------------
//Nreport =  Nsteps/150;
//Nreport =  10;
//Nreport =  2;

  Nreport = 10;
//Nreport = 250;

//Nreport = 1000;
//Nreport = 10000;

  //---------------------------------------------
  // set frequency of rendering
  //---------------------------------------------
  Nrender = Nreport;
//Nrender = 250;
//Nrender = 1000;

//NvtkInterp = 12;        // set output resolution
//NvtkInterp =  5;        // set output resolution
  NvtkInterp = this->N;   // use original nodes

  Summary();  // show simulation details
}


//---------------------------------------------------------
void CurvedINS2D::create_solvers()
//---------------------------------------------------------
{

#ifdef NDG_USE_CHOLMOD
  // allocate CHOLMOD solvers
  PRsystemC  = new CHOLMOD_solver;
  VELsystemC = new CHOLMOD_solver;
#else
  // allocate Cholesky solvers
  PRsystemC  = new CS_Chol;
  VELsystemC = new CS_Chol;
#endif

  // TODO: allow sparse LU solver for non-sym-pos-def

}


//---------------------------------------------------------
void CurvedINS2D::free_solvers()
//---------------------------------------------------------
{
  // clear Cholesky solvers
  if (PRsystemC)  { delete PRsystemC;  PRsystemC=NULL; }
  if (VELsystemC) { delete VELsystemC; VELsystemC=NULL; }
}


//---------------------------------------------------------
void CurvedINS2D::reset_solvers()
//---------------------------------------------------------
{
  // release and recreate Cholesky solvers
  free_solvers();
  create_solvers();
}


//---------------------------------------------------------
void CurvedINS2D::TimeScaleBCData() 
//---------------------------------------------------------
{
  // apply temporal scaling factors to bc data

  double tfac=1.0, tfac1=1.0, tpfac=1.0, tpfac1=1.0, tpfac2=1.0;

  if (eVolkerCylinder == sim_type) {
    tfac  = sin(pi* time/8.0);
    tfac1 = sin(pi*(time+dt)/8.0);
    tpfac = (pi/8.0)*cos(pi* time/8.0);
    tpfac1= (pi/8.0)*cos(pi*(time)/8.0);
    tpfac2= (pi/8.0)*cos(pi*(time)/8.0);
  } 
  else if (ePearsonVortex == sim_type) {
    double piSq=pi*pi;
    tfac  = exp(-nu*4*piSq*time);
    tfac1 = exp(-nu*4*piSq*(time+dt));
    tpfac = -4*nu*  piSq*exp(-nu*4*piSq*time);
    tpfac1= exp(-nu*8*piSq*(time));
    tpfac2= exp(-nu*8*piSq*(time+dt)); 
  }

  bcUx = tfac*refbcUx;     rhsbcUx = tfac1*refrhsbcUx; 
  bcUy = tfac*refbcUy;     rhsbcUy = tfac1*refrhsbcUy; 
  bcPR = tpfac1*refbcPR;   rhsbcPR = tpfac2*refrhsbcPR; 

  bcdUndt = tpfac*refbcdUndt;
}



//---------------------------------------------------------
void CurvedINS2D::Summary()
//---------------------------------------------------------
{
  // TODO: add details of operators and sparse solvers
  NDG2D::Summary();
}


//---------------------------------------------------------
void CurvedINS2D::Report(bool bForce)
//---------------------------------------------------------
{
  static DMat vort;
  bool normal_report=true;
  if (eVolkerCylinder == sim_type) {normal_report=false;}

#if (1)
  normal_report=true;
#endif

  // print report header on first step
  if (1 == tstep) {
    if (normal_report) {
      umLOG(1, "\n  step    time      min(Ux)    max(Ux)   min(Vort)    max(Vort)\n"
                 "--------------------------------------------------------------------\n");
    } else {
      umLOG(1, "\n  step     time        Cd/ra      Cl/ra       dP   \n"
                 "---------------------------------------------------\n");
    }
  }

  if (normal_report) {
    if (!umMOD(tstep,Nreport)||(1==tstep)||(tstep==Nsteps)||bForce) {

      Curl2D(Ux, Uy, vort);

      umLOG(1, "%7d  %6.3lf  %10.5lf %10.5lf  %10.2lf %10.2lf\n", 
               tstep, time, Ux.min_val(), Ux.max_val(), vort.min_val(), vort.max_val());
    }
  } else {
    // VolkerCylinder: compute coefficients of drag and lift,
    // as well as the pressure drop at the two sample points.
    INSLiftDrag2D(0.05);
  }


  if (!umMOD(tstep,Nrender)||(1==tstep)||(tstep==Nsteps)||bForce) 
  {
    // (this->*ExactSolution)(x, y, time, nu, exUx, exUy, exPR);

    Curl2D(Ux, Uy, vort);

    // load render data
    Q_plot.set_col(1, Ux); // -exUx);
    Q_plot.set_col(2, Uy); // -exUy);
    Q_plot.set_col(3, PR); // -exPR);
    Q_plot.set_col(4, vort);

    OutputVTK(Q_plot, NvtkInterp);
  }
}


//---------------------------------------------------------
void CurvedINS2D::FinalReport()
//---------------------------------------------------------
{
  // force report on final step
  // this->Report(true);

  // Report error and work info
  //if (HasAnalyticSol()) {
  //  umLOG(1, "\n Max analytic error : %12.4e\n", this->GetAnalyticError());
  //  umLOG(1,   "----------------------------------\n"); 
  //}

  umLOG(1, "\n operator setup :  %8.2lf seconds\n",  time_setup);
  umLOG(1,   "      advection :  %8.2lf\n", time_advection);
  umLOG(1,   "        viscous :  %8.2lf (chol %0.2lf)\n", time_viscous, time_viscous_sol);
  umLOG(1,   "       pressure :  %8.2lf (chol %0.2lf)\n", time_pressure, time_pressure_sol);
  umLOG(1,   " total NDG work :  %8.2lf\n",  time_work);
  umLOG(1,   " total sim time :  %8.2lf\n\n",time_total);
}

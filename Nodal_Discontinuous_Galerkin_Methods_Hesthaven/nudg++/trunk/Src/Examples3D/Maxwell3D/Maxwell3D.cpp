// Maxwell3D.cpp
// member routines for class Maxwell3D
// 2007/06/10
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "Maxwell3D.h"


//---------------------------------------------------------
Maxwell3D::Maxwell3D()
//---------------------------------------------------------
{
  class_name = "Maxwell3D-TM";
}


//---------------------------------------------------------
Maxwell3D::~Maxwell3D()
//---------------------------------------------------------
{
}


//---------------------------------------------------------
void Maxwell3D::Resize()
//---------------------------------------------------------
{
  // Allocate storage for member arrays

  // storage for solution
  Hx.resize(Np, K);    Hy.resize(Np, K);    Hz.resize(Np, K);
  Ex.resize(Np, K);    Ey.resize(Np, K);    Ez.resize(Np, K);
  Ezinit.resize(Np, K);

  // Runge-Kutta residual storage  
  resHx.resize(Np,K);  resHy.resize(Np,K);  resHz.resize(Np,K); 
  resEx.resize(Np,K);  resEy.resize(Np,K);  resEz.resize(Np,K);

  // field differences at faces
  int Nr = Nfp*Nfaces;
  dHx.resize(Nr,K);    dHy.resize(Nr,K);    dHz.resize(Nr,K);
  dEx.resize(Nr,K);    dEy.resize(Nr,K);    dEz.resize(Nr,K);

  // storage for output
  Q_plot.resize(Np*K, 3);
}


//---------------------------------------------------------
void Maxwell3D::SetIC()
//---------------------------------------------------------
{
  // Set initial conditions for simulation

  // Set initial conditions
  mmode = 1.0; nmode = 1.0;

  // Use TM mode Maxwell's initial condition 

  xmode = 1.0; ymode = 1.0; 
  DVec tsinx = apply(sin, (xmode*pi*x)),
       tsiny = apply(sin, (ymode*pi*y));

  Ezinit = tsinx.dm(tsiny);
//Ezinit = exp(-20.0 * (sqr(x) + sqr(y)));

  Hx=0.0;  Hy=0.0;  Hz=0.0;  
  Ex=0.0;  Ey=0.0;  Ez=Ezinit;
}


//---------------------------------------------------------
void Maxwell3D::SetStepSize()
//---------------------------------------------------------
{
  // compute time step size (dt)

  dt = dtscale3D();  // TW: buggy

  // correct dt for integer # of time steps
  Nsteps = (int)ceil(FinalTime/dt);
  dt = FinalTime/(double)Nsteps;
}


//---------------------------------------------------------
void Maxwell3D::InitRun()
//---------------------------------------------------------
{
  StartUp3D();          // construct grid and metric
  Resize();             // allocate work arrays
  SetIC();              // set initial conditions
  SetStepSize();        // calculate step size (dt)

  int Ns = Nsteps+10;   // expected number of samples
  sampleEz.resize(Ns);  // Ez(t) at sample point 
  sampleT.resize(Ns);   // time for each Ez(t)


  // just call base class version 
  NDG3D::InitRun();

  Nreport = 1;         // set frequency of reporting (param)
//Nreport = 50;         // set frequency of reporting (param)
//Nrender = Nreport;    // output frequency           (param)
  Nrender = 100000;     // output frequency           (param)

  Summary();            // show simulation details
}


//---------------------------------------------------------
void Maxwell3D::Summary()
//---------------------------------------------------------
{
  NDG3D::Summary();
}


//---------------------------------------------------------
void Maxwell3D::Report(bool bForce)
//---------------------------------------------------------
{
  if (1 == tstep) {
    // print header
  //umLOG(1, "\n** Adjust Nreport when timing **\n\n");
    umLOG(1, "\n step    time     Ezmin    Ezmax     Ezerr\n"
             "--------------------------------------------\n");
  }

  // sample Ez by interpolation 
  DVec ev = Ez(All,sampletet);
  sampleEz(tstep) = inner(sampleweights, ev);
  sampleT(tstep)  = tstep*dt;

  if (1 == tstep || !umMOD(tstep,Nreport) || bForce || Nsteps == tstep) {

    EzAnal = Ezinit * (cos(pi*sqrt(2.0)*time));
    errEz = Ez - EzAnal;
    m_maxAbsError = m_ErrAnalytic.max_val_abs();

    umLOG(1, "%5d  %7.3lf   %8.5lf %8.5lf  %g\n", 
              tstep, time, Ez.min_val(), Ez.max_val(), m_maxAbsError);
  }

  //#####################################
  // skip field output for timing tests
  //#####################################
  //return;


  if (!umMOD(tstep,Nrender) || bForce) {

#if (0)
     
    DMat MM = trans(invV)*invV;
    L2errEz(tstep) = 0.0;
    for (k=1:K) {
      ev = errEz(All,k);
      L2errEz(tstep) += ev*MM*(dm(J(All,k),ev));
    }

    if (!mod(tstep,40)) {
      clf; PlotContour3D(2*N, Ez, linspace(-.9, .9, 10));
      drawnow; pause(.02); 
    }

    //###########################################
#elif (0)
    //###########################################

    Q_plot.set_col(1, Hx);    // load plot data
    Q_plot.set_col(2, Hy);
    Q_plot.set_col(3, Ez);
    NvtkInterp = 12;          // set output resolution
  //NvtkInterp = this->N;     // set output resolution
    OutputVTK(Q_plot, NvtkInterp);

  //###########################################
#endif

  }
}


//---------------------------------------------------------
void Maxwell3D::FinalReport()
//---------------------------------------------------------
{
  // force report on final step
  // this->Report(true);

  // report work times (in seconds)
  umLOG(1, "\n time for NDG work  : %12.2lf secs\n", time_work);
  umLOG(1,   " time for RHS       : %12.2lf secs\n", time_rhs);
  umLOG(1,   " time for main loop : %12.2lf secs\n\n", time_total);
}

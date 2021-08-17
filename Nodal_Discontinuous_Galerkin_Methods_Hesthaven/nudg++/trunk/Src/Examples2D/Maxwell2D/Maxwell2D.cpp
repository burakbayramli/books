// Maxwell2D.cpp
// member routines for class Maxwell2D
// 2007/06/10
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "Maxwell2D.h"


//---------------------------------------------------------
Maxwell2D::Maxwell2D()
//---------------------------------------------------------
{
  class_name = "Maxwell2D-TM";
}


//---------------------------------------------------------
Maxwell2D::~Maxwell2D()
//---------------------------------------------------------
{
}


//---------------------------------------------------------
void Maxwell2D::Resize()
//---------------------------------------------------------
{
  // Allocate storage for member arrays

  // storage for solution
  Hx.resize(Np, K);
  Hy.resize(Np, K);
  Ez.resize(Np, K);
  Ezinit.resize(Np, K);

  // Runge-Kutta residual storage  
  resHx.resize(Np,K); 
  resHy.resize(Np,K); 
  resEz.resize(Np,K);

  // field differences at faces
  dHx.resize(Nfp*Nfaces,K);
  dHy.resize(Nfp*Nfaces,K);
  dEz.resize(Nfp*Nfaces,K);

  // storage for output
  Q_plot.resize(Np*K, 3);
}


//---------------------------------------------------------
void Maxwell2D::SetIC()
//---------------------------------------------------------
{
  // Set initial conditions for simulation

  mmode = 1.0; nmode = 1.0;

  //Ez = sin(mmode*pi*x) .* sin(nmode*pi*y);
  DVec tsinx = apply(sin, (mmode*pi*x)),
       tsiny = apply(sin, (nmode*pi*y));
  
  Ezinit = tsinx.dm(tsiny);

  Ez = Ezinit;
  Hx = 0.0;
  Hy = 0.0;
}


//---------------------------------------------------------
void Maxwell2D::SetStepSize()
//---------------------------------------------------------
{
  // compute time step size (dt)
  JacobiGQ(0.0, 0.0, N, rLGL, w);
  double rmin = std::abs(rLGL(1)-rLGL(2));
  dtscale2D(dtscale); dt = dtscale.min_val() * rmin * 2.0/3.0;

  Nsteps = (int)ceil(FinalTime/dt);
  dt = FinalTime/(double)Nsteps;

#ifndef NDEBUG
  string msg = rLGL.display("rLGL", 6, 10, true, 1, 'F');
  umLOG(1, "\n %s: N = %d,  dt = %8.6lf \n", this->GetClassName(), N, dt);
  umLOG(1, "\n ev = %s \n", msg.c_str());
#endif
}


//---------------------------------------------------------
void Maxwell2D::InitRun()
//---------------------------------------------------------
{
  StartUp2D();          // construct grid and metric

  Resize();             // allocate work arrays
  SetIC();              // set initial conditions
  SetStepSize();        // calculate step size (dt)

  // just call base class version 
  NDG2D::InitRun();

  //---------------------------------------------
  // Adjust reporting and render frequencies
  //---------------------------------------------
  Nreport =  Nsteps/20;
  Nrender = Nreport;    // output frequency           (param)

  NvtkInterp = 12;      // set output resolution

  Summary();            // show simulation details
}


//---------------------------------------------------------
void Maxwell2D::Summary()
//---------------------------------------------------------
{
  NDG2D::Summary();
}


//---------------------------------------------------------
void Maxwell2D::Report(bool bForce)
//---------------------------------------------------------
{
  if (1 == tstep) {
    // print header
    umLOG(1, "\n step    time     Ezmin    Ezmax\n"
             "----------------------------------\n");
  }

  if (1 == tstep || !umMOD(tstep,Nreport) || bForce) {
    umLOG(1, "%5d  %7.3lf   %8.5lf %8.5lf\n", 
              tstep, time, Ez.min_val(), Ez.max_val());
  }

  //#####################################
  // skip field output for timing tests
  //#####################################
  //return;


  if (!umMOD(tstep,Nrender) || bForce) {
    Q_plot.set_col(1, Hx);    // load plot data
    Q_plot.set_col(2, Hy);
    Q_plot.set_col(3, Ez);
    OutputVTK(Q_plot, NvtkInterp);
  }
}


//---------------------------------------------------------
void Maxwell2D::FinalReport()
//---------------------------------------------------------
{
  // force report on final step
  this->Report(true);

  // report work times (in seconds)
  umLOG(1, "\n time for NDG work  : %12.2lf secs\n", time_work);
  umLOG(1,   " time for RHS       : %12.2lf secs\n", time_rhs);
  umLOG(1,   " time for main loop : %12.2lf secs\n\n", time_total);
}

// Euler3D.cpp
// member routines for class Euler3D
// 2007/07/11
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "Euler3D.h"


//---------------------------------------------------------
Euler3D::Euler3D()
//---------------------------------------------------------
{
  class_name = "Euler3D-TM";

  // set simulation parameters
  gamma = 1.4;
  gm1   = gamma - 1.0;

  // toggle use of cut-off filter
  m_bApplyFilter = false;
}


//---------------------------------------------------------
Euler3D::~Euler3D()
//---------------------------------------------------------
{
}


//---------------------------------------------------------
void Euler3D::Resize()
//---------------------------------------------------------
{
  // Allocate storage for member arrays

  int Nr  = Np*K;           // volume arrays
  int Nrf = Nfp*Nfaces*K;   // face arrays

  // storage for solution, RHS and residual
  Q.resize   (Np*K, 5);
  resQ.resize(Np*K, 5);
  rhsQ.resize(Np*K, 5);

  // allocate storage for volume flux data.
  // Note: not using high-order cubature nodes
  cF.resize(Nr,5); cG.resize(Nr,5); cH.resize(Nr,5);

  QM.resize(Nrf,5);  QP.resize(Nrf,5); flux.resize(Nrf,5);
  fM.resize(Nrf,5);  gM.resize(Nrf,5);  hM.resize(Nrf,5);
  fP.resize(Nrf,5);  gP.resize(Nrf,5);  hP.resize(Nrf,5);
}


//---------------------------------------------------------
void Euler3D::SetIC()
//---------------------------------------------------------
{
  // Set initial conditions for simulation
  (this->*InitialSolution)(x, y, z, 0.0, Q);
}


//---------------------------------------------------------
void Euler3D::SetStepSize()
//---------------------------------------------------------
{
  // function dt = EulerDT3D(Q, gamma)
  // purpose: compute the time step dt for the compressible Euler equations

  DVec q1,q2,q3,q4,q5, u,v,w,p,c, rho,rhou,rhov,rhow,Ener;
  DVec squvw, Fscale_2,w_speeds;
  int Nr = Q.num_rows(); 

  // wrap current state data (columns of Q)
  q1.borrow(Nr, Q.pCol(1)); q2.borrow(Nr, Q.pCol(2)); 
  q3.borrow(Nr, Q.pCol(3)); q4.borrow(Nr, Q.pCol(4));
  q5.borrow(Nr, Q.pCol(5));

  rho = q1(vmapM); rhou = q2(vmapM); rhov = q3(vmapM); rhow = q4(vmapM); Ener = q5(vmapM);
  u = rhou.dd(rho); v = rhov.dd(rho); w = rhow.dd(rho); squvw = sqr(u)+sqr(v)+sqr(w);

  p = gm1*(Ener - rho.dm(squvw)/2.0);
  c = sqrt(abs(gamma*p.dd(rho)));

  Fscale_2 = 0.5*Fscale;

  w_speeds=SQ(N+1)*Fscale_2.dm(sqrt(squvw)+c);
  dt = 1.0/w_speeds.max_val();

  Nsteps = (int)ceil(FinalTime/dt);
  //dt = FinalTime/(double)Nsteps;
}


//---------------------------------------------------------
void Euler3D::PrecalcBdryData()
//---------------------------------------------------------
{

  //---------------------------------------------
  // pre-calculate data for IsentropicVortexBC3D
  //---------------------------------------------
  if (eIsentropicVortex == sim_type) {
    gmapB = concat(mapI, mapO, mapW);
  //gxB = m_gauss.x(gmapB);
  //gyB = m_gauss.y(gmapB);
    gxB =   this->Fx(gmapB);
    gyB =   this->Fy(gmapB);
    gzB =   this->Fz(gmapB);
  }

  //---------------------------------------------
  // pre-calculate data for CouetteBC3D
  //---------------------------------------------
  else if (eCouetteFlow == sim_type)  {

    //#####################################################
    // FIXME: what about top and bottom of 3D annulus?
    //#####################################################
    gmapB = concat(mapI, mapO);

  //gxB = m_gauss.x(gmapB);
  //gyB = m_gauss.y(gmapB);
    gxB =   this->Fx(gmapB);
    gyB =   this->Fy(gmapB);
    gzB =   this->Fz(gmapB);


#if (1)
    // FIXME: atan2(y, 0.0);
    for (int i=1; i<=gxB.size(); ++i) {
      if (fabs(gxB(i))<1e-22) 
      {
        if ( gxB(i) >= 0.0 )
             gxB(i) =  1e-22;
        else gxB(i) = -1e-22;
      }
    }
#endif


    DVec   rad2B = sqr(gxB) + sqr(gyB);
    DVec    radB = sqrt(rad2B);
    DVec  thetaB = atan2(gyB, gxB);
    DVec uthetaB = (-radB + 16.0/radB)/75.0;
    DVec      pB = 1.0 + (1.0/SQ(75.0)) * (rad2B/2.0 - 32.0*log(radB) - 128.0/rad2B);

    // store constant boundary data
    int Nr= gmapB.size();
    rhoB  = ones(Nr);
    rhouB = (-sin(thetaB)).dm(uthetaB);
    rhovB = ( cos(thetaB)).dm(uthetaB);
    rhowB.zeros(Nr);
    EnerB = pB/gm1 + 0.5*(sqr(rhouB)+sqr(rhovB)+sqr(rhowB)).dd(rhoB);

  }
}


//---------------------------------------------------------
void Euler3D::InitRun()
//---------------------------------------------------------
{
  StartUp3D();          // construct grid and metric
  Resize();             // allocate work arrays
  BuildBCMaps3D();      // map boundary nodes
  SetIC();              // set initial conditions
  SetStepSize();        // calculate step size (dt)

  PrecalcBdryData();    // precalculate any constant BC data

  NDG3D::InitRun();     // base class sets timers/counters

//Nreport = 10;         // set frequency of reporting (param)
  Nreport = 20;         // set frequency of reporting (param)
//Nreport = 50;         // set frequency of reporting (param)
//Nrender = Nreport;    // output frequency           (param)
  Nrender = 100000;     // output frequency           (param)

  Summary();            // show simulation details
}


//---------------------------------------------------------
void Euler3D::Summary()
//---------------------------------------------------------
{
  NDG3D::Summary();
}


//---------------------------------------------------------
void Euler3D::Report(bool bForce)
//---------------------------------------------------------
{
  if (1 == tstep) {
    // print header
  //umLOG(1, "\n step   time     dt     min(ro) max(ro)  min(En)  max(En)\n"
  //           "----------------------------------------------------------------\n");
    umLOG(1, "\n step   time     dt       min(ro)   max(ro)     min(En)   max(En)\n"
               "----------------------------------------------------------------------\n");
  }

  if (!umMOD(tstep,Nreport) || bForce) // || (1==tstep)) 
  {
    double r_min=Q.min_col_val(1), r_max=Q.max_col_val(1);
    double e_min=Q.min_col_val(5), e_max=Q.max_col_val(5);
  //umLOG(1, "[%4d] %0.4f  %0.4f   %6.4f  %6.4f    %6.4f  %6.4f\n", 
    umLOG(1, "[%4d] %0.5lf  %0.5lf   %8.6lf  %8.6lf    %8.6lf  %8.6lf\n", 
              tstep,time,   dt,    r_min, r_max,   e_min, e_max);
  }


#if (0)
  //#######################################################
  timeanddt = [tstep, time, dt]
  %EulerRender3D(Q, gamma, time, ExactSolution);
  clf; 
  subplot(1,2,1); PlotContour3D(N, Q(:,:,1), linspace(.1, .95, 10));  colorbar; axis on; 
  title(sprintf('Density @ t=%f',time)); drawnow; pause(.05);

  p = (gamma-1)*(Q(:,:,5) - 0.5*(Q(:,:,2).^2+Q(:,:,3).^2+Q(:,:,4).^2)./Q(:,:,1));
  subplot(1,2,2); PlotContour3D(N, p, linspace(-.5, .5, 10));  colorbar; axis on; 
  title(sprintf('Pressure @ t=%f',time)); drawnow; pause(.05);

  exactQ = feval(ExactSolution, x, y, z, time);

  densError = max(max(abs(exactQ(:,:,1)-Q(:,:,1))))
  momxError = max(max(abs(exactQ(:,:,2)-Q(:,:,2))))
  momyError = max(max(abs(exactQ(:,:,3)-Q(:,:,3))))
  momzError = max(max(abs(exactQ(:,:,4)-Q(:,:,4))))
  enerError = max(max(abs(exactQ(:,:,5)-Q(:,:,5))))
  //#######################################################
#endif


  if (!umMOD(tstep,Nrender) || bForce) {

#if (0)
    Q_plot = Q;
    // extract conserved variables, calculate 
    // vorticity, and load this as 4th field:
    DMat rho, rhou,rhov,rhow, u,v,w, curl;
    rho.borrow(Np,K,Q.pCol(1)); rhou.borrow(Np,K,Q.pCol(2)); rhov.borrow(Np,K,Q.pCol(3));
    u = rhou.dd(rho); v = rhov.dd(rho);
    Curl3D(u,v,w, curl);
    Q_plot(All,5) = curl;  
    OutputVTK(Q_plot, NvtkInterp);
#elif (0)
    OutputVTK(Q, NvtkInterp);
#endif

  }
}


//---------------------------------------------------------
void Euler3D::FinalReport()
//---------------------------------------------------------
{
  // force report on final step
  //this->Report(true);

  // report work times (in seconds)
  umLOG(1, "\n time for NDG work  : %12.2lf secs\n", time_work);
  umLOG(1,   " time for RHS       : %12.2lf secs\n", time_rhs);
  umLOG(1,   " time for main loop : %12.2lf secs\n\n", time_total);
}

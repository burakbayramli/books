// EulerShock2D.cpp
// member routines for class EulerShock2D
// 2007/08/15
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "EulerShock2D.h"


//---------------------------------------------------------
EulerShock2D::EulerShock2D()
//---------------------------------------------------------
{
  class_name = "EulerShock2D";

  // set simulation parameters
  gamma = 1.4;
  gm1 = gamma - 1.0;

  // number of h-refinements of default mesh
  Nrefine = 0;

  // use IC routine for first iteration
  bDoCalcIC = true;
  mesh_level = 0;
}


//---------------------------------------------------------
EulerShock2D::~EulerShock2D()
//---------------------------------------------------------
{
}


//---------------------------------------------------------
void EulerShock2D::Resize()
//---------------------------------------------------------
{
  // (re)allocate member arrays

  if (mesh_level <= 1) {    // only resize on first iteration
    Q.resize (Np*K,  4);    // solution fields
  }
  rhsQ.resize(Np*K,  4);    // Runge-Kutta stage values
  resQ.resize(Np*K,  4);    // Runge-Kutta residual 
}


//---------------------------------------------------------
void EulerShock2D::SetIC()
//---------------------------------------------------------
{
  if (bDoCalcIC) 
  {
    // compute initial condition (time=0)
    // Q = feval(InitialSolution, x, y, 0.0);
    (this->*InitialSolution)(     x, y, 0.0, Q);

    // next iteration we will use simulation state
    bDoCalcIC = false;
  }
}


//---------------------------------------------------------
void EulerShock2D::SetStepSize()
//---------------------------------------------------------
{
  // call base class version
  CurvedEuler2D::SetStepSize();
}


//---------------------------------------------------------
void EulerShock2D::InitRun()
//---------------------------------------------------------
{
  // base class performs usual startup sequence
  // CurvedEuler2D::InitRun();

  //-------------------------------------
  // construct grid and metric
  //-------------------------------------
  StartUp2D();

  //-------------------------------------
  // refine default mesh
  //-------------------------------------
  if (Nrefine>=1) {
    umLOG(1, "before refinement K = %6d\n", K);
    for (int i=1; i<=Nrefine; ++i) {
      DMat Qtmp(Np*K, 1);  IMat refineflag;
      refineflag = Ones(K,Nfaces); Qtmp = ConformingHrefine2D(refineflag, Qtmp);
      umLOG(1, "after h-refine %d: K = %6d\n", i,K);
    }
  }

  // store original BC types before adjusting, 
  // (e.g. BC_Cyl faces may be set to BC_Wall)
  saveBCType = BCType;

  //-------------------------------------
  // Adjust faces on circular boundaries
  //-------------------------------------
  switch (sim_type) {

  case eForwardStep:
    // no cylinder faces
    straight.range(1,K); curved.resize(0);
    break;

  case eScramInlet:
    // no cylinder faces
    straight.range(1,K); curved.resize(0);
    break;
  
  default:
    // set default maps for {straight,curved} elements
    straight.range(1,K); curved.resize(0);
    break;
  }

  BuildBCMaps2D();  // map faces subject to boundary conditions
  Resize();         // allocate arrays
  SetIC();          // set initial conditions

#if (1)
  OutputNodes(false); // volume nodes
#endif


#if (0)
  tstep = -1;
  Report(true);
#endif

  SetStepSize();    // compute initial timestep (using IC's)

  // storage for residual at each time-step,
  // allowing for variable step size
  resid.resize(2*Nsteps);

  // base class version sets counters and flags
  NDG2D::InitRun();   

  // pre-calculate constant data for limiter routine
  precalc_limiter_data();

  //---------------------------------------------
  // Adjust reporting and render frequencies
  //---------------------------------------------

  switch (sim_type) {

  case eForwardStep:
    Nreport     = 100;    // frequency of reporting
    Nrender     = 300;    // frequency of rendering
    NvtkInterp  = 3;      // resolution of vtk output
    break;

  case eScramInlet:
    NvtkInterp = 2;
    switch (mesh_level) {
    case 1:   Nreport =  100;  Nrender =  100;   break;
    case 2:   Nreport =  250;  Nrender =  250;   break;
    case 3:   Nreport =  500;  Nrender =  500;   break;
    case 4:   Nreport = 1000;  Nrender = 1000;   break;
    default:  Nreport = 1000;  Nrender = 1000;   break;
    }
    break;
  }

  // Show simulation details
  Summary();
}


//---------------------------------------------------------
void EulerShock2D::precalc_limiter_data()
//---------------------------------------------------------
{

  //---------------------------------------------
  // pre-calculate element geometry and constant 
  // factors for use in this->EulerLimiter2D()
  //---------------------------------------------

  Lim_AVE = 0.5 * MassMatrix.col_sums();
  DMat dropAVE = eye(Np) - outer(ones(Np),Lim_AVE);
  Lim_dx = dropAVE*x; 
  Lim_dy = dropAVE*y;

  // Extract coordinates of vertices of elements
  IVec v1=EToV(All,1);  Lim_xv1=VX(v1); Lim_yv1=VY(v1);
  IVec v2=EToV(All,2);  Lim_xv2=VX(v2); Lim_yv2=VY(v2);
  IVec v3=EToV(All,3);  Lim_xv3=VX(v3); Lim_yv3=VY(v3);  

  const DVec &xv1=Lim_xv1,&xv2=Lim_xv2,&xv3=Lim_xv3;
  const DVec &yv1=Lim_yv1,&yv2=Lim_yv2,&yv3=Lim_yv3;
  DMat &fnx=Lim_fnx, &fny=Lim_fny, &fL=Lim_fL;

  // Compute face unit normals and lengths
  fnx.resize(3,K); fny.resize(3,K);

  // fnx = (3,K) = [yv2-yv1; yv3-yv2; yv1-yv3];
  fnx.set_row(1, yv2-yv1); fnx.set_row(2, yv3-yv2); fnx.set_row(3, yv1-yv3);

  // fny = (3,K) =  -[xv2-xv1;xv3-xv2;xv1-xv3];
//fny.set_row(1, xv2-xv1); fny.set_row(2, xv3-xv2); fny.set_row(3, xv1-xv3);
  fny.set_row(1, xv1-xv2); fny.set_row(2, xv2-xv3); fny.set_row(3, xv3-xv1);

  fL = sqrt(sqr(fnx)+sqr(fny)); fnx.div_element(fL); fny.div_element(fL);

  //-------------------------------------------------------
  // Compute coords of element centers and face weights
  //-------------------------------------------------------

  // Find neighbors in patch
  Lim_E1=EToE(All,1); Lim_E2=EToE(All,2); Lim_E3=EToE(All,3);

  // Compute coordinates of element centers
  xc0=Lim_AVE*x; xc1=xc0(Lim_E1); xc2=xc0(Lim_E2); xc3=xc0(Lim_E3);
  yc0=Lim_AVE*y; yc1=yc0(Lim_E1); yc2=yc0(Lim_E2); yc3=yc0(Lim_E3);

  // Compute weights for face gradients 
  A0=Lim_AVE*J*TWOTHIRD;
  A1=A0+A0(Lim_E1); A2=A0+A0(Lim_E2); A3=A0+A0(Lim_E3);
  A1_A2_A3 = A1+A2+A3;

  // Find boundary faces for each face 
  Lim_id1=find(BCType.get_col(1),'!',0);
  Lim_id2=find(BCType.get_col(2),'!',0);
  Lim_id3=find(BCType.get_col(3),'!',0);

  // Compute location of centers of reflected ghost elements at boundary faces
  if (1) {
    DMat FL1=fL(1,Lim_id1), Fnx1=fnx(1,Lim_id1), Fny1=fny(1,Lim_id1);
    DVec fL1=FL1, fnx1=Fnx1, fny1=Fny1;
    DVec H1   = 2.0*(dd(A0(Lim_id1),fL1));
    xc1(Lim_id1) += 2.0*fnx1.dm(H1);
    yc1(Lim_id1) += 2.0*fny1.dm(H1);

    DMat FL2=fL(2,Lim_id2), Fnx2=fnx(2,Lim_id2), Fny2=fny(2,Lim_id2);
    DVec fL2=FL2, fnx2=Fnx2, fny2=Fny2;
    DVec H2   = 2.0*(dd(A0(Lim_id2),fL2));
    xc2(Lim_id2) += 2.0*fnx2.dm(H2);
    yc2(Lim_id2) += 2.0*fny2.dm(H2);

    DMat FL3=fL(3,Lim_id3), Fnx3=fnx(3,Lim_id3), Fny3=fny(3,Lim_id3);
    DVec fL3=FL3, fnx3=Fnx3, fny3=Fny3;
    DVec H3   = 2.0*(dd(A0(Lim_id3),fL3));
    xc3(Lim_id3) += 2.0*fnx3.dm(H3);
    yc3(Lim_id3) += 2.0*fny3.dm(H3);
  }

  // Find boundary faces
  IVec bct = trans(BCType);
  Lim_idI = find(bct, '=', (int)BC_In);
  Lim_idO = find(bct, '=', (int)BC_Out);
  Lim_idW = find(bct, '=', (int)BC_Wall);
  Lim_idC = find(bct, '=', (int)BC_Cyl);

  Lim_ctx.resize(3,K); Lim_cty.resize(3,K);
  Lim_ctx.set_row(1,xc1); Lim_ctx.set_row(2,xc2); Lim_ctx.set_row(3,xc3);
  Lim_cty.set_row(1,yc1); Lim_cty.set_row(2,yc2); Lim_cty.set_row(3,yc3);

  // load the set of ids
  Lim_ids.resize(6);
  Lim_ids(1)=1;      Lim_ids(2)=Nfp;    Lim_ids(3)=Nfp+1;
  Lim_ids(4)=2*Nfp;  Lim_ids(5)=3*Nfp;  Lim_ids(6)=2*Nfp+1;

  limQ = Q;
}


//---------------------------------------------------------
void EulerShock2D::Summary()
//---------------------------------------------------------
{
  // TODO: add details of operators and sparse solvers

  // call base class version
  CurvedEuler2D::Summary();
}


//---------------------------------------------------------
void EulerShock2D::Report(bool bForce)
//---------------------------------------------------------
{
  CurvedEuler2D::Report(bForce);

  if (tstep>=1 && tstep <= resid.size()) 
  {
    // calculate residual
    // resid(tstep) = sqrt(sum(sum(sum((Q-oldQ).^2)))/(4*K*Np));
    // resid(tstep) = sqrt(sum(sum(sum((Q-oldQ).^2)))/(4*K*Np))/dt;


    DMat Qresid = sqr(Q-oldQ); double d4KNp = double(4*K*Np);
    resid(tstep) = sqrt(Qresid.sum()/d4KNp);

    if (eScramInlet == sim_type) {
      // scale residual
      resid(tstep) /= dt;
    }
  }
}


//---------------------------------------------------------
void EulerShock2D::FinalReport()
//---------------------------------------------------------
{
  // force report on final step
  this->Report(true);

  // Report error and work info
  if (HasAnalyticSol()) {
    umLOG(1, "\n Max analytic error : %12.4e\n", this->GetAnalyticError());
    umLOG(1,   "----------------------------------\n"); 
  }

  umLOG(1, "\n time for NDG work  : %12.5lf secs\n",  time_work);
  umLOG(1,   "           rhs work : %12.5lf\n",       time_rhs);
  umLOG(1, "\n time for Flux calc : %12.5lf secs\n",  time_flux);
  umLOG(1,   " time for RHS - Flux: %12.5lf secs\n",  time_rhs-time_flux);
  umLOG(1,   " time for Limiter   : %12.5lf secs\n",  time_limit);
  umLOG(1,   " time for main loop : %0.2lf secs\n\n", time_total);

#if (0)
  resid.print(stderr, "resid", "e", 4, 12, false, 3);
#endif
}

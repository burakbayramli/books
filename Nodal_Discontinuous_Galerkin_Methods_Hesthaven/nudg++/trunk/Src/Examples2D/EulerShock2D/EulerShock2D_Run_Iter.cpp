// EulerShock2D_Run_Iter.cpp
// function [Q,resid] = AdaptiveEuler2D(tol, fluxtype)
// Purpose  : Integrate 2D Euler equations using a 
//            2nd order SSP Runge-Kutta time integrator
//            Stop if residual drops below tol
// 2007/08/15
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "EulerShock2D.h"

// http://www.lcp.nrl.navy.mil/cfd-cta/CFD3/problems/inlet/index.html

//---------------------------------------------------------
void EulerShock2D::Run_Iter()
//---------------------------------------------------------
{
  bool bStart = 1;
  int change = 1;
  double residtol = 5e-4;   // 1e-5;
  double jumptol  = 7.5e-3; // 5e-3

//FinalTime = 15;   // 2007/08/15
  FinalTime =  5;   // d(max rho) ~1e3 near t=5.0

  for (mesh_level=1; mesh_level<=6; ++mesh_level)
  {
    switch (mesh_level) {
      case 1: FinalTime = 5; break;
      case 2: FinalTime = 3; break;
      case 3: FinalTime = 1; break;
      case 4: FinalTime = 1; break;
      case 5: FinalTime = 1; break;
      case 6: FinalTime = 1; break;
      default: FinalTime = 1; break;
    }

    InitRun();

    // choose order to integrate exactly
    CubatureOrder = (int)floor(2.0*(N+1)*3.0/2.0);
    NGauss        = (int)floor(2.0*(N+1));

    // build cubature node data for all elements
    CubatureVolumeMesh2D(CubatureOrder);

    // build Gauss node data for all element faces
    GaussFaceMesh2D(NGauss);

    Resize_cub();         // resize cubature arrays
  //MapGaussFaceData();   // {nx = gauss.nx}, etc.
    ti0=timer.read();     // time simulation loop

  #if (0)
    //-------------------------------------
    // check all node sets
    //-------------------------------------
    OutputNodes(false);   // volume nodes
    OutputNodes(true);    // face nodes
    OutputNodes_cub();    // cubature
    OutputNodes_gauss();  // quadrature
    Report(true);         // show initial conditions
    umLOG(1, "\n*** Exiting after Cub, Gauss\n\n");
    return;
  #endif


    // limit initial condition
    Q = EulerLimiter2D(Q, time);

    // outer time step loop 
    while (time<FinalTime) {

      if (time+dt > FinalTime) {dt=FinalTime-time;}
      tw1=timer.read();   // time NDG work
      oldQ = Q;           // store solutuion from previous step

      // 2nd order SSP Runge-Kutta
      this->RHS(Q,  time, BCSolution); Q1 =  Q +      dt*rhsQ;      Q1 = EulerLimiter2D(Q1, time);
      this->RHS(Q1, time, BCSolution); Q  = (Q + Q1 + dt*rhsQ)/2.0; Q  = EulerLimiter2D(Q,  time);
      
      time += dt;         // increment time 
      SetStepSize();      // compute new timestep
      time_work += timer.read() - tw1;  // accumulate cost of NDG work

      Report();           // optional reporting
      if (resid(tstep)<residtol) {  // Check convergence:
        break;            // move to next iteration
      }

      ++tstep;            // increment timestep

      // if (tstep>=10) break;      // testing
    }

    // setup for next iteration
    AdaptMesh_Mach(jumptol);
  }

  time_total = timer.read()-ti0;  // stop timing
  FinalReport();                  // final report
}


//---------------------------------------------------------
void EulerShock2D::AdaptMesh_Mach(double jumptol)
//---------------------------------------------------------
{
  // adapt the mesh based on jumps in mach number

  DVec rho,rhou,rhov,Ener,p,sqrurv,mach; 
  DMat jumpmach; IVec jumpIds; IMat refineflag;
  int Nr = Q.num_rows();
  rho.borrow (Nr, Q.pCol(1)); rhou.borrow(Nr, Q.pCol(2));
  rhov.borrow(Nr, Q.pCol(3)); Ener.borrow(Nr, Q.pCol(4));

  sqrurv = sqr(rhou) + sqr(rhov);
  p = gm1*(Ener-0.5*(sqrurv)/rho);
  mach = sqrt(sqrurv)/(gamma*p);

  // compute relative jumps and compare with allowed tolerance
  jumpmach = (abs(mach(vmapP)-mach(vmapM)) + 1e-20) / (abs(mach(vmapM))+1e-10);
  jumpmach.reshape(Nfp*Nfaces, K);

  // tolvec = (max(jumpmach, [], 1)>jumptol);
//jumpIds = find(jumpmach.max_col_vals(), '>', jumptol);
  jumpIds.assign((jumpmach.max_col_vals()).gt(jumptol));

  // Flag all faces of any  element with any face in default of jump tolerance
  refineflag = outer(Ones(Nfaces), jumpIds);


#if (0)
  dumpIVec(jumpIds, "jumpIds");
  dumpIMat(refineflag, "refineflag");
  umERROR("Nigel", "Check jumpIds");
#endif


#if (0)
  //#######################################################
  // extend refinement to neighbors of elements flagged to be refined
  ids = find(refineflag(1,:));
  refineflag(:,EToE(ids,:)) = 1;
  //#######################################################
#endif

  int Korig = K; BCType = saveBCType;
  Q = ConformingHrefine2D(trans(refineflag), Q); 

  int change = K - Korig;
  umMSG(1, "K(old) = %d   K(new) = %d  change = %d\n", Korig, K, change);
}
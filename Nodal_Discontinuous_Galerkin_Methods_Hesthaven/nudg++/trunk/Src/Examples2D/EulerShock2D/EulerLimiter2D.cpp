// EulerLimiter2D.m
// function [LQ] = EulerLimiter2D(Q, SolutionBC, time)
// 2007/07/03
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "EulerShock2D.h"


//---------------------------------------------------------
DMat& EulerShock2D::EulerLimiter2D(const DMat& Qin, double ti)
//---------------------------------------------------------
{
  //---------------------------
  double t1 = timer.read();
  //---------------------------

  // function [LQ] = EulerLimiter2D(Q, SolutionBC, time)
  // Purpose: limit the Euler solution using slope limiting adapted from 
  // A SLOPE LIMITING PROCEDURE IN DISCONTINUOUS GALERKIN FINITE ELEMENT METHOD FOR 
  // GASDYNAMICS APPLICATIONS. SHUANGZHANG TU AND SHAHROUZ ALIABADI 
  // INTERNATIONAL JOURNAL OF NUMERICAL ANALYSIS AND MODELING, Volume 2, Number 2, Pages 163
  //
  // Gass constant, gamma = 1.4;

//const double epse  = 1e-10;
  const double epse  = 1e-14;

  // 1. compute geometric information for 4 element patch containing each element
  // Build average matrix, AVE = sum(MassMatrix)/2;

  // Compute displacements from center of nodes for Taylor expansion of limited fields

  // Note: these factors are precalculated in InitRun()
  const DVec &AVE=Lim_AVE; const DMat &dx=Lim_dx, &dy=Lim_dy;
  const DVec &xv1=Lim_xv1, &xv2=Lim_xv2, &xv3=Lim_xv3;
  const DVec &yv1=Lim_yv1, &yv2=Lim_yv2, &yv3=Lim_yv3;
  const DMat &fnx=Lim_fnx, &fny=Lim_fny, &fL=Lim_fL;
  const DMat &ctx=Lim_ctx, &cty=Lim_cty;
  const IVec &E1 =Lim_E1,  &E2 =Lim_E2, &E3=Lim_E3;
  const IVec &id1=Lim_id1, &id2=Lim_id2, &id3=Lim_id3;
  DMat& Qref = const_cast<DMat&>(Qin);

  static DMat PC0, pc, pva;

  DMat rho, rhou, rhov, Ener;  // borrow columns of Qin as (Np,K) matrices
  DVec onesNp = ones(Np);

  // 2. Find cell averages of conserved & primitive variables in each 4 element patch
  // extract fields from Q

  rho.borrow (Np,K,Qref.pCol(1)); rhou.borrow(Np,K,Qref.pCol(2));
  rhov.borrow(Np,K,Qref.pCol(3)); Ener.borrow(Np,K,Qref.pCol(4));

  // Compute cell averages of conserved variables
  DVec rhoC=AVE*rho, rhouC=AVE*rhou, rhovC=AVE*rhov, EnerC=AVE*Ener;

  DMat averhou = outer(onesNp,rhouC), averhov = outer(onesNp,rhovC);
  DMat averho  = outer(onesNp,rhoC),  aveEner = outer(onesNp,EnerC);

  // Compute primitive variables from cell averages of conserved variables
  PC0.resize(rhoC.size(),4);
  PC0(All,1)=rhoC; PC0(All,2)=rhouC/rhoC; PC0(All,3)=rhovC/rhoC;  
  PC0(All,4)=gm1*(EnerC - 0.5*(sqr(rhouC) + sqr(rhovC))/rhoC);

  // Find neighbor values of conserved variables
  IMat e2eT = trans(EToE); pc.resize(e2eT.size(),4);
  pc(All,1)=rhoC(e2eT); pc(All,2)=rhouC(e2eT); pc(All,3)=rhovC(e2eT); pc(All,4)=EnerC(e2eT);

  // Apply boundary conditions to cell averages of ghost cells at boundary faces
  (this->*BCSolution)(Lim_ctx, Lim_cty, Lim_fnx, Lim_fny, Lim_idI, Lim_idO, Lim_idW, Lim_idC, ti, pc);

  DMat PC[5], PVA[5], aV[5], dV[5];
  PC[1].borrow(3,K, pc.pCol(1)); PC[2].borrow(3,K, pc.pCol(2));
  PC[3].borrow(3,K, pc.pCol(3)); PC[4].borrow(3,K, pc.pCol(4));

  PC[2].div_element(PC[1]);
  PC[3].div_element(PC[1]);
  PC[4]=gm1*(PC[4] - 0.5*PC[1].dm(sqr(PC[2]) + sqr(PC[3])));

  DMat t_rA,t_ruA,t_rvA,t_EnA, rhoA,rhouA,rhovA,EnerA;
  DMat uA("uA"), vA("vA"), pA("pA");

  // 3. Compute average of primitive variables at face nodes

  t_rA  = (0.5*(rho (vmapP) + rho (vmapM)));  t_rA.reshape (Nfp*Nfaces, K);  rhoA  = t_rA (Lim_ids,All);
  t_ruA = (0.5*(rhou(vmapP) + rhou(vmapM)));  t_ruA.reshape(Nfp*Nfaces, K);  rhouA = t_ruA(Lim_ids,All);
  t_rvA = (0.5*(rhov(vmapP) + rhov(vmapM)));  t_rvA.reshape(Nfp*Nfaces, K);  rhovA = t_rvA(Lim_ids,All);
  t_EnA = (0.5*(Ener(vmapP) + Ener(vmapM)));  t_EnA.reshape(Nfp*Nfaces, K);  EnerA = t_EnA(Lim_ids,All);

  uA = rhouA.dd(rhoA); vA = rhovA.dd(rhoA);
  pA = gm1*(EnerA - 0.5*rhoA.dm(sqr(uA) + sqr(vA)));

  int Nff = Nfp*Nfaces;
  pva.resize(Nff*K,4);
  pva(All,1)=rhoA; pva(All,2)=uA; pva(All,3)=vA; pva(All,4)=pA; 
  PVA[1].borrow(Nff,K, pva.pCol(1)); PVA[2].borrow(Nff,K, pva.pCol(2));
  PVA[3].borrow(Nff,K, pva.pCol(3)); PVA[4].borrow(Nff,K, pva.pCol(4));


  // 4. Apply limiting procedure to each of the primitive variables

  // Storage for cell averagse and limited gradients at each node of each element
  // aV = zeros(Np,K,4); dV = zeros(Np,K,4);

  DVec dVdxE1,dVdyE1,dVdxE2,dVdyE2,dVdxE3,dVdyE3;
  DVec dVdxC0, dVdxC1, dVdxC2, dVdxC3, LdVdxC0;
  DVec dVdyC0, dVdyC1, dVdyC2, dVdyC3, LdVdyC0;
  DVec va1,va2,va3,va4,va5,va6, g1,g2,g3, w1,w2,w3, fac,fac3e;


  // Loop over primitive variables
  for (int n=1; n<=4; ++n) 
  {
    DVec VC0,VC1,VC2,VC3;  DMat VA;

    // find value of primitive variables in patches
    VC0 = PC0(All,n);         // PC0(1,:,n);
    VC1 = PC[n].get_row(1);   // PC (1,:,n);
    VC2 = PC[n].get_row(2);   // PC (2,:,n);
    VC3 = PC[n].get_row(3);   // PC (3,:,n);
    VA  = PVA[n];             // PVA(:,:,n);

    va1=VA.get_row(1);  va2=VA.get_row(2);  va3=VA.get_row(3);
    va4=VA.get_row(4);  va5=VA.get_row(5);  va6=VA.get_row(6);

    // Compute face gradients
    dVdxE1 =  0.5*( (VC1-VC0)*(yv2-yv1) + (va1-va2)*(yc1-yc0) )/A1;
    dVdyE1 = -0.5*( (VC1-VC0)*(xv2-xv1) + (va1-va2)*(xc1-xc0) )/A1;
    dVdxE2 =  0.5*( (VC2-VC0)*(yv3-yv2) + (va3-va4)*(yc2-yc0) )/A2;
    dVdyE2 = -0.5*( (VC2-VC0)*(xv3-xv2) + (va3-va4)*(xc2-xc0) )/A2;
    dVdxE3 =  0.5*( (VC3-VC0)*(yv1-yv3) + (va5-va6)*(yc3-yc0) )/A3;
    dVdyE3 = -0.5*( (VC3-VC0)*(xv1-xv3) + (va5-va6)*(xc3-xc0) )/A3;

    dVdxC0 = (A1*dVdxE1 + A2*dVdxE2 + A3*dVdxE3) / (A1_A2_A3);
    dVdyC0 = (A1*dVdyE1 + A2*dVdyE2 + A3*dVdyE3) / (A1_A2_A3);
    
    dVdxC1 = dVdxC0(E1); dVdxC2 = dVdxC0(E2); dVdxC3 = dVdxC0(E3);
    dVdyC1 = dVdyC0(E1); dVdyC2 = dVdyC0(E2); dVdyC3 = dVdyC0(E3);

    // Use face gradients at ghost elements
    dVdxC1(id1) = dVdxE1(id1); dVdxC2(id2) = dVdxE2(id2); dVdxC3(id3) = dVdxE3(id3);
    dVdyC1(id1) = dVdyE1(id1); dVdyC2(id2) = dVdyE2(id2); dVdyC3(id3) = dVdyE3(id3);

    // Build weights used in limiting
    g1 = (sqr(dVdxC1) + sqr(dVdyC1));
    g2 = (sqr(dVdxC2) + sqr(dVdyC2));
    g3 = (sqr(dVdxC3) + sqr(dVdyC3));

    fac = sqr(g1)+sqr(g2)+sqr(g3);  fac3e = fac+3.0*epse;

    w1 = (g2*g3 + epse) / fac3e;
    w2 = (g1*g3 + epse) / fac3e;
    w3 = (g1*g2 + epse) / fac3e;

    // Limit gradients
    LdVdxC0 = w1*dVdxC1 + w2*dVdxC2 + w3*dVdxC3;
    LdVdyC0 = w1*dVdyC1 + w2*dVdyC2 + w3*dVdyC3;

    // Evaluate limited gradient and cell averages at all nodes of each element
    dV[n] = dx.dm(outer(onesNp,LdVdxC0)) + dy.dm(outer(onesNp,LdVdyC0));
    aV[n] = outer(onesNp,VC0);
  }

  DMat aveu,avev,avep, drho,du,dv,dp, Lrho,Lrhou,Lrhov,Lp;
  IVec rids, pids;

  // 5. Reconstruct conserved variables using cell averages and limited gradients  
  averho = aV[1]; aveu = aV[2]; avev = aV[3]; avep = aV[4];
  drho   = dV[1];   du = dV[2];   dv = dV[3];   dp = dV[4];

  // Reconstruct and check for small densities and/or pressures
  double rtol = 1e-2;   // tolerance for density
  double ptol = 1e-2;   // tolerance for pressure

  Lrho = averho + drho; rids = find(Lrho.min_col_vals(),'<',rtol);
  int riter=0;
  while (rids.size()>0) {
    umMSG(1, "warning: correcting negative Rho ** in [%3d] elements\n", rids.size());
    drho(All,rids) *= 0.5;
    Lrho = averho + drho; rids = find(Lrho.min_col_vals(),'<',rtol);
    if (++riter>3 && rids.size()>0) {
      umMSG(1, "*** forcing average density in  [%3d] elements\n", rids.size());
      Lrho(All,rids) = averho(All,rids);
      break;
    }
  }

  // Reconstruct momentum
  Lrhou = averhou + averho.dm(du) + drho.dm(aveu);
  Lrhov = averhov + averho.dm(dv) + drho.dm(avev);

  // Reconstruct energy
  DMat dEner = (1.0/gm1)*dp + 0.5*drho.dm(sqr(aveu)+sqr(avev)) + averho.dm(aveu.dm(du)+avev.dm(dv));
  DMat LEner = aveEner + dEner;

  // Check for negative pressures => zero gradient 
  Lp = gm1*(LEner - 0.5*(sqr(Lrhou) + sqr(Lrhov)).dd(Lrho));
  pids = find(Lp.min_col_vals(),'<',ptol);
  if (pids.size() > 0) {
    umMSG(1, "correcting negative pressure in [%3d] elements\n", pids.size());
    int piter = 0;
    while (pids.size()>0 && piter<3) {
      // NBN reduce the change in pressure, and recalc energy
      dp(All,pids) *= 0.5;
      dEner = (1.0/gm1)*dp + 0.5*drho.dm(sqr(aveu)+sqr(avev)) + averho.dm(aveu.dm(du)+avev.dm(dv));
      LEner = aveEner + dEner;
      Lp = gm1*(LEner - 0.5*(sqr(Lrhou) + sqr(Lrhov)).dd(Lrho));
      pids = find(Lp.min_col_vals(),'<',ptol);
      ++piter;
      if (piter>=3 && pids.size()>0) {
        umMSG(1, "*** forcing average pressure in [%3d] elements\n", pids.size());
        LEner(All,pids) = aveEner(All,pids);
      }
    }
  }

  // Replace limited gradients with face gradient at boundary faces
  limQ(All,1)=Lrho; limQ(All,2)=Lrhou; limQ(All,3)=Lrhov; limQ(All,4)=LEner;

  //---------------------------
  time_limit += timer.read() - t1;
  //---------------------------

  return limQ;
}

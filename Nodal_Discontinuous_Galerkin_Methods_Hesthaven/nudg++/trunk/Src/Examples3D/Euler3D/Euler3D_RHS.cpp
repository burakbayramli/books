// Euler3D_RHS.m
// function [rhsQ] = EulerRHS3D(Qin,time, ExactSolutionBC);
// 2007/07/11
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "Euler3D.h"

//---------------------------------------------------------
void Euler3D::RHS(DMat& Qin, double ti, fp_BC SolutionBC)
//---------------------------------------------------------
{
  // function [rhsQ] = EulerRHS3D(Qin,time);
  // Purpose: Evaluate RHS in 3D Euler equations, discretized 
  //          on weak form with a local Lax-Friedrich flux

  trhs = timer.read();  // time RHS work

  DMat dFdr,dFds,dFdt, dGdr,dGds,dGdt, dHdr,dHds,dHdt; int n=0;
  DMat Fn,Gn,Hn,lambda,nflux;  DVec rhoM,uM,vM,wM,pM, rhoP,uP,vP,wP,pP;

  // 1. Compute volume contributions (INDEPENDENT OF SURFACE TERMS)
  this->Fluxes(Qin, cF,cG,cH);

  // Compute weak derivatives
  for (n=1; n<=5; ++n) {
    Fn.borrow(Np,K, cF.pCol(n)); Gn.borrow(Np,K, cG.pCol(n)); Hn.borrow(Np,K, cH.pCol(n));

    dFdr = Drw*Fn;  dFds = Dsw*Fn;  dFdt = Dtw*Fn;
    dGdr = Drw*Gn;  dGds = Dsw*Gn;  dGdt = Dtw*Gn;
    dHdr = Drw*Hn;  dHds = Dsw*Hn;  dHdt = Dtw*Hn;

    rhsQ(All,n) = 
        ( rx.dm(dFdr) + sx.dm(dFds) + tx.dm(dFdt) ) +
        ( ry.dm(dGdr) + sy.dm(dGds) + ty.dm(dGdt) ) +
        ( rz.dm(dHdr) + sz.dm(dHds) + tz.dm(dHdt) );
  }

  // 2. Compute surface contributions 
  // 2.1 evaluate '-' and '+' traces of conservative variables
  int Nr = Qin.num_rows(); DVec Qn("Qn");
  for (n=1; n<=5; ++n) {
    Qn.borrow (Nr,Qin.pCol(n));
    QM(All,n) = Qn(vmapM);
    QP(All,n) = Qn(vmapP);
  }

  // 2.2 set boundary conditions by modifying positive traces
  if (SolutionBC) {
    (this->*BCSolution)(Fx,Fy,Fz, nx,ny,nz, mapI,mapO,mapW,mapC, ti, QP);
  }

  // 2.3 evaluate primitive variables & flux functions at '-' and '+' traces
  this->Fluxes(QM, fM,gM,hM, rhoM,uM,vM,wM,pM);
  this->Fluxes(QP, fP,gP,hP, rhoP,uP,vP,wP,pP);

  // 2.4 Compute local Lax-Friedrichs/Rusonov numerical fluxes
  lambda = max( sqrt(sqr(uM)+sqr(vM)+sqr(wM)) + sqrt(abs(gamma*pM.dd(rhoM))),
	              sqrt(sqr(uP)+sqr(vP)+sqr(wP)) + sqrt(abs(gamma*pP.dd(rhoP))));

  lambda.reshape(Nfp, Nfaces*K);
  lambda = outer(ones(Nfp), lambda.max_col_vals());
  lambda.reshape(Nfp*Nfaces, K);

  // 2.5 Lift fluxes
  for (n=1; n<=5; ++n) {
    nflux = 
        nx.dm(fP(All,n) + fM(All,n)) + 
        ny.dm(gP(All,n) + gM(All,n)) + 
        nz.dm(hP(All,n) + hM(All,n)) + 
        lambda.dm(QM(All,n) - QP(All,n));

    rhsQ(All,n) -= 0.5*LIFT*(Fscale.dm(nflux));
  }

  time_rhs += (timer.read() - trhs);
}

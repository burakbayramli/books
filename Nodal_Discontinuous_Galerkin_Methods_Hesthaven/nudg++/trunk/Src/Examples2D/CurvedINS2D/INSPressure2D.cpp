// INSPressure2D.m
// perform the pressure step for the incompressible Navier-Stokes solver
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedINS2D.h"

//---------------------------------------------------------
void CurvedINS2D::INSPressure2D()
//---------------------------------------------------------
{
  DMat DivUT,CurlU,dCurlUdx,dCurlUdy,res1,res2,dPRdx,dPRdy;
  DVec PRrhs;  double t1 = timer.read(), t2,t3,t4;

  // compute divergence of UxT and UyT
  Div2D(UxT, UyT,  DivUT);

  // dp/dn = -n.(u.grad u + curl curl u)
  Curl2D(Ux,Uy, CurlU);
  Grad2D(CurlU, dCurlUdx, dCurlUdy);

  // NBN: reordered to reduce flops
  res1 =  -nu*dCurlUdy - NUx;
  res2 =   nu*dCurlUdx - NUy;

  // save old and compute new dp/dn
  dpdnold = dpdn;  dpdn = 0.0;

  // apply Neumann boundary conditions for pressure 
  // to boundary nodes mapped by {nbcmapD,vbcmapD}. 
  // these arrays are set in precalc_data().

  // NBN: FiXME:  
  if (eBackdrop != sim_type) {
    dpdn(nbcmapD) = dm(nx(nbcmapD), res1(vbcmapD)) + dm(ny(nbcmapD), res2(vbcmapD)); 
    dpdn -= bcdUndt;
  }
  
  // Evaluate right hand side term for Poisson equation for pressure
  PRrhs = MassMatrix*(J.dm(-DivUT*(g0/dt)) + LIFT*(sJ.dm(b0*dpdn + b1*dpdnold)));

  // Add Dirichlet boundary condition forcing
  PRrhs += rhsbcPR;

  // Pressure Solve (select Cholesky, CG, LU, GMRES solvers)
  // [-laplace PR = +(div UT)/dt + LIFT*dpdn] on boundaries
  t2 = timer.read();
  PR = PRsystemC->solve(PRrhs);
  t3 = timer.read();

  // compute  (U~~,V~~) = (U~,V~) - dt*grad PR
  Grad2D(PR, dPRdx,dPRdy);

  // increment (Ux~,Uy~) to (Ux~~,Uy~~)
  UxTT = UxT - dPRdx*(dt/g0);
  UyTT = UyT - dPRdy*(dt/g0);

  //---------------------------
  t4 = timer.read();
  time_pressure_sol += t3 - t2;
  time_pressure     += t4 - t1;
  //---------------------------
}

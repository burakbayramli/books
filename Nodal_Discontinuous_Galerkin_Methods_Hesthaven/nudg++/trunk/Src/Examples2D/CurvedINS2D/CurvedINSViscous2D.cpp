// CurvedINSViscous2D.m
// compute right hand side for viscous step solves
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedINS2D.h"


//---------------------------------------------------------
void CurvedINS2D::CurvedINSViscous2D()
//---------------------------------------------------------
{
  double t1 = timer.read(), t2,t3;

  // compute right hand side for viscous step solves
  DMat mmUxTT = (m_cub.VT)*(m_cub.W.dm(m_cub.V*UxTT));
  Uxrhs  = mmUxTT*(g0/(nu*dt)) + rhsbcUx;
  
  DMat mmUyTT = (m_cub.VT)*(m_cub.W.dm(m_cub.V*UyTT));
  Uyrhs  = mmUyTT*(g0/(nu*dt)) + rhsbcUy;

  // save Ux,Uy
  Uxold = Ux; Uyold = Uy;

  // viscous solves (Cholesky, CG, LU, GMRES solvers)
  t2 = timer.read();
  Ux = VELsystemC->solve(Uxrhs);
  Uy = VELsystemC->solve(Uyrhs);
  t3 = timer.read();

  //---------------------------
  time_viscous_sol += t3 - t2;
  time_viscous     += t3 - t1;
  //---------------------------
}

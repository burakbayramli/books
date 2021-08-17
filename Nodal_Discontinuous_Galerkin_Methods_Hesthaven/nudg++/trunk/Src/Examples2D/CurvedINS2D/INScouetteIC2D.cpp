// INScouetteIC2D.cpp
// function [Ux, Uy, PR] = INScouetteIC2D(x, y, time, nu)
// 2007/07/14
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedINS2D.h"

//---------------------------------------------------------
void CurvedINS2D::INScouetteIC2D
(
  const DVec&   xi, 
  const DVec&   yi, 
        double  ti,   // [in]
        double  nu,   // [in]
        DMat&   Uxo,  // [out]
        DMat&   Uyo,  // [out]
        DMat&   PRo   // [out]
)
//---------------------------------------------------------
{
  // function [Ux, Uy, PR] = INScouetteIC2D(x, y, time, nu)
  // Purpose: evaluate initial solution for Taylor-Couette 
  //          flow with inner/outer cylinders at {r=1,r=4}
  //          (gamma = 1.4)

  DVec   rad2 = sqr(xi) + sqr(yi);
  DVec   rad  = sqrt(rad2);
  DVec  theta = atan2(yi, xi);
  DVec utheta = (-rad + 16.0/rad)/75.0;

#if (0)
  // set initial velocity {u,v} to steady-state solution
  Uxo = (-sin(theta)).dm(utheta);   // ? / rho;
  Uyo = ( cos(theta)).dm(utheta);
  PRo = 1.0 + (1.0/SQ(75.0)) * (rad2/2.0 - 32.0*log(rad) - 128.0/rad2);
#else
  // set initial velocity {u,v} to zero
  Uxo.zeros(Np,K);  // = 0.0;
  Uyo.zeros(Np,K);  // = 0.0;
  PRo = 1.0 + (1.0/SQ(75.0)) * (rad2/2.0 - 32.0*log(rad) - 128.0/rad2);
#endif
}

// CouetteIC2D.m
// function [Q] = CouetteIC2D(x, y, time)
// 2007/06/21
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedEuler2D.h"


//---------------------------------------------------------
void CurvedEuler2D::CouetteIC2D
(
  const DVec&   xi,   // [in]
  const DVec&   yi,   // [in]
        double  ti,   // [in]
        DMat&   Qo    // [out]
)
//---------------------------------------------------------
{
  // function [Q] = CouetteIC2D(x, y, time)
  // Purpose: evaluat solution for Couette flow

  // gamma = 1.4;
  int Nr = Qo.num_rows(); DVec rho,rhou,rhov,Ener, q1,q2,q3;
  double gm1 = gamma-1.0;

  DVec   rad2 = sqr(xi) + sqr(yi);
  DVec   rad  = sqrt(rad2);
  DVec  theta = atan2(yi, xi);
  DVec utheta = (-rad + 16.0/rad)/75.0;
  DVec      p = 1.0 + (1.0/SQ(75.0)) * (rad2/2.0 - 32.0*log(rad) - 128.0/rad2);

#if (0)
  // set initial {rho,rhou,rhov} to steady-state solution
  Qo(All,1) =  1.0;
  Qo(All,2) = (-sin(theta)).dm(utheta);
  Qo(All,3) = ( cos(theta)).dm(utheta);
#else
  // set initial {rho,rhou,rhov} with zero momentum
  Qo(All,1) =  1.0;
  Qo(All,2) = 0.0;
  Qo(All,3) = 0.0;
#endif

  // extract initial {rho,rhou,rhov} from Q(:,1:3) 
  q1.borrow(Nr,Qo.pCol(1)); q2.borrow(Nr,Qo.pCol(2)); q3.borrow(Nr,Qo.pCol(3));

  // use initial {rho,rhou,rhov} to calculate initial {Ener}
  Qo(All,4) =  p/gm1 + 0.5*(sqr(q2)+sqr(q3)).dd(q1);
}

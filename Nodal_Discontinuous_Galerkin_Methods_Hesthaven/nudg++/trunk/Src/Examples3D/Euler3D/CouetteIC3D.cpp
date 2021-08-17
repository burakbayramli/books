// CouetteIC3D.cpp
// function Q = CouetteIC3D(x, y, z, ti)
// 2007/07/11
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "Euler3D.h"


//---------------------------------------------------------
void Euler3D::CouetteIC3D
(
  const DVec&   xi,   // [in]
  const DVec&   yi,   // [in]
  const DVec&   zi,   // [in]
        double  ti,   // [in]
        DMat&   Qo    // [out]
)
//---------------------------------------------------------
{
  //#######################################################
  // FIXME: what about top and bottom of 3D annulus?
  //#######################################################


  // function [Q] = CouetteIC2D(x, y, time)
  // Purpose: evaluat solution for Couette flow

  // gamma = 1.4;
  int Nr = Qo.num_rows(); DVec rho,rhou,rhov,rhow,Ener, q1,q2,q3,q4;

  DVec   rad2 = sqr(xi) + sqr(yi);
  DVec   rad  = sqrt(rad2);

#if (1)
  // FIXME: atan2(y, 0.0);
  DVec  xi_fix = xi;
  for (int i=1; i<=xi.size(); ++i) {
    if (fabs(xi_fix(i))<1e-22)
    {
      if ( xi_fix(i) >= 0.0 )
           xi_fix(i) =  1e-22;
      else xi_fix(i) = -1e-22;
    }
  }
#endif

//DVec  theta = atan2(yi, xi);
  DVec  theta = atan2(yi, xi_fix);
  DVec utheta = (-rad + 16.0/rad)/75.0;
  DVec      p = 1.0 + (1.0/SQ(75.0)) * (rad2/2.0 - 32.0*log(rad) - 128.0/rad2);

#if (0)
  // set initial {rho,rhou,rhov,rhow} to steady-state solution
  Qo(All,1) = 1.0;
  Qo(All,2) = (-sin(theta)).dm(utheta);
  Qo(All,3) = ( cos(theta)).dm(utheta);
  Qo(All,4) = 0.0;
#else
  // set initial {rho,rhou,rhov,rhow} with zero momentum
  Qo(All,1) = 1.0;
  Qo(All,2) = 0.0;
  Qo(All,3) = 0.0;
  Qo(All,4) = 0.0;
#endif

  // extract initial {rho,rhou,rhov} from Q(:,1:3) 
  q1.borrow(Nr,Qo.pCol(1));
  q2.borrow(Nr,Qo.pCol(2));
  q3.borrow(Nr,Qo.pCol(3));
  q4.borrow(Nr,Qo.pCol(4));

  // use initial {rho,rhou,rhov} to calculate initial {Ener}
  Qo(All,5) =  p/gm1 + 0.5*(sqr(q2)+sqr(q3)+sqr(q4)).dd(q1);
}

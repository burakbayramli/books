// dtscale3D.m
// function dtscale = dtscale3D; 
// 2007/06/11
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG3D.h"


//---------------------------------------------------------
double NDG3D::dtscale3D() const
//---------------------------------------------------------
{
  // function dtscale = dtscale3D;
  // Purpose : Compute inscribed sphere diameter as characteristic
  //           for grid to choose timestep

  return 1.0 / (Fscale.max_val()*N*N);
}

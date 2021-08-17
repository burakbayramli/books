// BoxFlowBC2D.m
// function [rho,rhou,rhov,Ener] = BoxFlowBC2D(rho, rhou, rhov, Ener, ti)
// 2007/06/21
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedCNS2D.h"

//---------------------------------------------------------
void CurvedCNS2D::BoxFlowBC2D
(
  const DVec& rho,
  const DVec& rhou, 
  const DVec& rhov, 
  const DVec& Ener, 
        double rkti, 
        DVec& brho, 
        DVec& brhou, 
        DVec& brhov, 
        DVec& bEner
)
//---------------------------------------------------------
{
  // function function [rho,rhou,rhov,Ener] = ... 
  //      BoxFlowBC2D(rho, rhou, rhov, Ener, time)  
  //
  // Purpose: compute plane flow configuration 

  // nothing
  int Nr = rho.size();
  brho.zeros (Nr);
  brhou.zeros(Nr);
  brhov.zeros(Nr);
  bEner.zeros(Nr);
}

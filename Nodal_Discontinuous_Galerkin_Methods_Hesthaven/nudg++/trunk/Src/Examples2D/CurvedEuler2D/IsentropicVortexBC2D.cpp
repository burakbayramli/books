// IsentropicVortexBC2D.m
// function [Q] = IsentropicVortexBC2D(Q);
// 2007/06/30
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedEuler2D.h"


//---------------------------------------------------------
void CurvedEuler2D::IsentropicVortexBC2D
(
  const DVec& xi, 
  const DVec& yi, 
  const DVec& nxi, 
  const DVec& nyi, 
  const IVec& tmapI, 
  const IVec& tmapO, 
  const IVec& tmapW, 
  const IVec& tmapC, 
        double ti, 
        DMat& Qio
)
//---------------------------------------------------------
{
  // function [Q] = Euler2Dbc(Q)
  // Purpose: Impose boundary conditions on 2D Euler equations on weak form

  // Example is static isentropic vortex

  static DMat Qbc;  Qbc.resize(Qio);  DVec qn, qbcn;
  
  // Load Qbc with IsentropicVortex2D for time=ti
  IsentropicVortexIC2D(xi, yi, ti, Qbc);

  // map isentropic vortex values onto boundary nodes
  // Note: this loop updates Qio(:,n) in-place:
  int Nr=Qio.num_rows();
  for (int n=1; n<=4; ++n) {
    qn.borrow  (Nr, Qio.pCol(n));   // qn -> Q(:,n)
    qbcn.borrow(Nr, Qbc.pCol(n));
    qn(gmapB) = qbcn(gmapB);
  }
}

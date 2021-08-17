// PartialLiftData3D.cpp
// function [VM, gradVM, VP, gradVP] = ...
//      PartialLiftData3D(k1, f1, k2, f2, xg, yg, zg)
// 2007/10/08
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG3D.h"


//---------------------------------------------------------
void NDG3D::PartialLiftData3D
(
  int k1, int f1,   // [in]
  int k2, int f2,   // [in]
  const DVec& xg,   // [in]
  const DVec& yg,   // [in]
  const DVec& zg,   // [in]
        DMat& VM,   // [out]
  DMat gradVM[4],   // [out]
        DMat& VP,   // [out]
  DMat gradVP[4]    // [out]
)
//---------------------------------------------------------
{
  // function [VM, gradVM, VP, gradVP]=...
  //      PartialLiftData3D(k1, f1, k2, f2, xg, yg, zg)
  // purpose: compute traces of basis functions and their gradients
   
  DMat_Diag Dgrx,Dgsx,Dgtx, Dgry,Dgsy,Dgty, Dgrz,Dgsz,Dgtz;
  DVec grx,gsx,gtx,gry,gsy,gty,grz,gsz,gtz,gJ;
  DVec xki,yki,zki, r1,s1,t1, r2,s2,t2;

  // find local coordinates of Gauss nodes on each neighbor
  FindLocalCoords3D(k1, xg, yg, zg, r1,s1,t1);
  FindLocalCoords3D(k2, xg, yg, zg, r2,s2,t2);

  // evaluate Lagrange interpolants and derivatives at Gauss points
  VM = Vandermonde3D(N, r1,s1,t1)*invV; 
  VP = Vandermonde3D(N, r2,s2,t2)*invV; 

  //-------------------------------------------------------
  // calc. geometric factors at minus limit Gauss points
  //-------------------------------------------------------
  DMat DrM = VM*Dr,  DsM = VM*Ds,  DtM = VM*Dt;
  xki.borrow(Np,x.pCol(k1)); yki.borrow(Np,y.pCol(k1)); zki.borrow(Np,z.pCol(k1));
  ::GeometricFactors3D(xki,yki,zki, DrM,DsM,DtM, grx,gsx,gtx, gry,gsy,gty, grz,gsz,gtz, gJ);

  int Nr = grx.length();
  // wrap {-} vectors as diagonal matrices
  Dgrx.borrow(grx); Dgsx.borrow(gsx); Dgtx.borrow(gtx);
  Dgry.borrow(gry); Dgsy.borrow(gsy); Dgty.borrow(gty);
  Dgrz.borrow(grz); Dgsz.borrow(gsz); Dgtz.borrow(gtz);

  // use chain rule for computing - gradient in physical coordinates
  gradVM[1] = Dgrx*DrM + Dgsx*DsM + Dgtx*DtM;
  gradVM[2] = Dgry*DrM + Dgsy*DsM + Dgty*DtM;
  gradVM[3] = Dgrz*DrM + Dgsz*DsM + Dgtz*DtM;

  //-------------------------------------------------------
  // calc. geometric factors at plus limit Gauss points
  //-------------------------------------------------------
  DMat DrP = VP*Dr,  DsP = VP*Ds,  DtP = VP*Dt;
  xki.borrow(Np,x.pCol(k2)); yki.borrow(Np,y.pCol(k2)); zki.borrow(Np,z.pCol(k2));
  ::GeometricFactors3D(xki,yki,zki, DrP,DsP,DtP, grx,gsx,gtx, gry,gsy,gty, grz,gsz,gtz, gJ);

  // wrap {+} vectors as diagonal matrices
  Dgrx.borrow(grx); Dgsx.borrow(gsx); Dgtx.borrow(gtx);
  Dgry.borrow(gry); Dgsy.borrow(gsy); Dgty.borrow(gty);
  Dgrz.borrow(grz); Dgsz.borrow(gsz); Dgtz.borrow(gtz);

  // use chain rule for computing + gradient in physical coordinates
  gradVP[1] = Dgrx*DrP + Dgsx*DsP + Dgtx*DtP;
  gradVP[2] = Dgry*DrP + Dgsy*DsP + Dgty*DtP;
  gradVP[3] = Dgrz*DrP + Dgsz*DsP + Dgtz*DtP;
}

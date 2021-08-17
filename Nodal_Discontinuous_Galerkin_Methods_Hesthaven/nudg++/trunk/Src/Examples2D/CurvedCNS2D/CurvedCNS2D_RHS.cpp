// CurvedCNS2DRHS.m
// function [rhsQ] = CurvedCNSRHS2D(Q, mu, rkti, SolutionBC, fluxtype)
// 2007/06/25
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CurvedCNS2D.h"



//---------------------------------------------------------
void CurvedCNS2D::RHS(double rkti, fp_BC SolutionBC)
//---------------------------------------------------------
{
  // function [rhsQ] = CurvedCNSRHS2D(Q, mu, rkti, SolutionBC, fluxtype)
  // Purpose: evaluate right hand side residual of the 
  //          compressible Navier-Stokes equations

  trhs = timer.read();  // time RHS work

  DMat   rho,  rhou,  rhov,  Ener;    // state data
  DMat  crho, crhou, crhov, cEner;    // ... at cubature nodes
  DMat  grho, grhou, grhov, gEner;    // ... at quadrature nodes
  DMat  gu,gv,gu2gv2,gPr, cu,cv,cPr;  // primitive variables

  DMat  drhodx, drhoudx, drhovdx,  drhody, drhoudy, drhovdy;
  DMat cdrhodx,cdrhovdx,cdrhoudx, cdrhody,cdrhoudy,cdrhovdy, cdudx,cdudy,cdvdx,cdvdy;
  DMat gdrhodx,gdrhoudx,gdrhovdx, gdrhody,gdrhoudy,gdrhovdy, gdudx,gdudy,gdvdx,gdvdy;

  DVec  bu,bv,bPr,buB,bvB,bcNdotU , sqrB;
  static DVec brho, brhou, brhov, bEner;    // ... adjusted for boundary conditions
  DVec brhoB,brhouB,brhovB,bEnerB;    // mapped subset of boundary nodes

  // shorthand references
  Cub2D& cub = this->m_cub; Gauss2D& gauss = this->m_gauss;

  // Extract fields from two dimensional array of state data
  rho.borrow (Np,K, Q.pCol(1));
  rhou.borrow(Np,K, Q.pCol(2));
  rhov.borrow(Np,K, Q.pCol(3));
  Ener.borrow(Np,K, Q.pCol(4));

  // Interpolate fields to volume cubature nodes & Gauss quadrature surface nodes
  crho  = cub.V*rho;  grho  = gauss.interp*rho;
  crhou = cub.V*rhou; grhou = gauss.interp*rhou;
  crhov = cub.V*rhov; grhov = gauss.interp*rhov;
  cEner = cub.V*Ener; gEner = gauss.interp*Ener;

  // Compute primitive fields at Gauss quadrature surface nodes
  gu=grhou.dd(grho); gv=grhov.dd(grho);  gu2gv2 = sqr(gu)+sqr(gv);
  gPr=gm1*(gEner-0.5*grho.dm(gu2gv2));

  if (this->BCSolution) {

    // create boundary condition variables
    (this->*BCSolution)(grho, grhou, grhov, gEner, rkti, brho,brhou,brhov,bEner);

    if (eCouetteFlow == sim_type) {
      // use pre-calculated boundary data
      brhoB=rhoB; brhouB=rhouB; brhovB=rhovB; bEnerB=EnerB;
    } else {
      // gmapB = gauss.mapB;
      brhoB=brho(gmapB); brhouB=brhou(gmapB); brhovB=brhov(gmapB); bEnerB=bEner(gmapB);
    }

    // compute primitive variables of boundary data
    bu  = gu;   bu (gmapB) = brhouB.dd(brhoB);   buB=bu(gmapB);
    bv  = gv;   bv (gmapB) = brhovB.dd(brhoB);   bvB=bv(gmapB);
    bPr = gPr;  bPr(gmapB) = gm1*(bEnerB - 0.5*brhoB.dm(sqr(buB)+sqr(bvB)));
  }

  // Compute gradients of the conserved variables
  CurvedDGGrad2D(crho,  grho,  gmapB, brho,   drhodx,   drhody);
  CurvedDGGrad2D(crhou, grhou, gmapB, brhou,  drhoudx, drhoudy);
  CurvedDGGrad2D(crhov, grhov, gmapB, brhov,  drhovdx, drhovdy);

  //-------------------------------------------------------

  // Compute primitive fields at cubature nodes & Gauss quadrature surface nodes
  cu = crhou.dd(crho); cv=crhov.dd(crho); cPr = gm1*(cEner - 0.5*crho.dm(sqr(cu)+sqr(cv)));

  // Interpolate derivatives of conserved variables to cubature nodes
  cdrhodx  = cub.V*drhodx;  cdrhody  = cub.V*drhody; 
  cdrhoudx = cub.V*drhoudx; cdrhoudy = cub.V*drhoudy; 
  cdrhovdx = cub.V*drhovdx; cdrhovdy = cub.V*drhovdy; 

  // Use product-rule to evaluate gradients of velocity components at cubature nodes
  cdudx = (cdrhoudx - cdrhodx.dm(cu)).dd(crho);
  cdudy = (cdrhoudy - cdrhody.dm(cu)).dd(crho);
  cdvdx = (cdrhovdx - cdrhodx.dm(cv)).dd(crho);
  cdvdy = (cdrhovdy - cdrhody.dm(cv)).dd(crho);

  // Compute viscous stress tensor at cubature nodes
  DMat ct11 = mu * (2.0*cdudx - TWOTHIRD*(cdudx+cdvdy)); 
  DMat ct12 = mu * (    cdudy + cdvdx);
  DMat ct22 = mu * (2.0*cdvdy - TWOTHIRD*(cdudx+cdvdy)); 
  DMat ct31 = cu.dm(ct11) + cv.dm(ct12);
  DMat ct32 = cu.dm(ct12) + cv.dm(ct22);

  //-------------------------------------------------------

  // Interpolate derivatives of conserved variables to Gauss nodes
  gdrhodx  = gauss.interp*drhodx;  gdrhody  = gauss.interp*drhody; 
  gdrhoudx = gauss.interp*drhoudx; gdrhoudy = gauss.interp*drhoudy; 
  gdrhovdx = gauss.interp*drhovdx; gdrhovdy = gauss.interp*drhovdy; 

  // Use product-rule to evaluate gradients of velocity components at Gauss nodes
  gdudx = (gdrhoudx - gdrhodx.dm(gu)).dd(grho);
  gdudy = (gdrhoudy - gdrhody.dm(gu)).dd(grho);
  gdvdx = (gdrhovdx - gdrhodx.dm(gv)).dd(grho);
  gdvdy = (gdrhovdy - gdrhody.dm(gv)).dd(grho);

  // Compute viscous stress tensor at Gauss nodes
  DMat gt11 = mu * (2.0*gdudx - TWOTHIRD*(gdudx + gdvdy)); 
  DMat gt12 = mu * (gdudy + gdvdx);
  DMat gt22 = mu * (2.0*gdvdy - TWOTHIRD*(gdudx + gdvdy)); 
  DMat gt31 = gu.dm(gt11) + gv.dm(gt12);
  DMat gt32 = gu.dm(gt12) + gv.dm(gt22);

  //-------------------------------------------------------

  // Local copy of normals at Gauss nodes 
  DMat gnx=gauss.nx, gny=gauss.ny;

  // Add mass conservation terms together and compute divergence
  DMat cF = -crhou,  cG = -crhov;
  DMat gF = -grhou,  gG = -grhov;
  DVec bF = -brhou,  bG = -brhov;
  bcNdotU = gnx.dm(bF)+gny.dm(bG);
  rhsQ(All,1) = CurvedDGDiv2D(cF, cG, gF, gG, gmapB, bcNdotU);

  // Add x-momentum conservation terms together and compute divergence
  cF = -(crhou.dm(cu) + cPr) + ct11; cG = -(crhou.dm(cv)) + ct12;
  gF = -(grhou.dm(gu) + gPr) + gt11; gG = -(grhou.dm(gv)) + gt12;
  bF = -(brhou.dm(bu) + bPr) + gt11; bG = -(brhou.dm(bv)) + gt12;
  bcNdotU = gnx.dm(bF)+gny.dm(bG);
  rhsQ(All,2) = CurvedDGDiv2D(cF, cG, gF, gG, gmapB, bcNdotU);

  // Add y-momentum conservation terms together and compute divergence
  cF = -(crhou.dm(cv)) + ct12; cG = -(crhov.dm(cv) + cPr) + ct22;
  gF = -(grhou.dm(gv)) + gt12; gG = -(grhov.dm(gv) + gPr) + gt22;
  bF = -(brhou.dm(bv)) + gt12; bG = -(brhov.dm(bv) + bPr) + gt22;
  bcNdotU = gnx.dm(bF) +gny.dm(bG);
  rhsQ(All,3) = CurvedDGDiv2D(cF, cG, gF, gG, gmapB, bcNdotU);

  // Add Energy conservation terms together and compute divergence
  cF = cu.dm(-cEner - cPr) + ct31;  cG = cv.dm(-cEner - cPr) + ct32;
  gF = gu.dm(-gEner - gPr) + gt31;  gG = gv.dm(-gEner - gPr) + gt32;
  bF = bu.dm(-bEner - bPr) + gt31;  bG = bv.dm(-bEner - bPr) + gt32;
  bcNdotU = gnx.dm(bF)+gny.dm(bG);
  rhsQ(All,4) = CurvedDGDiv2D(cF, cG, gF, gG, gmapB, bcNdotU);

  //-------------------------------------------------------

  // Add Lax-Friedrichs jump stabilization
  NGauss = gauss.NGauss; DMat glambda("glambda");
  glambda = sqrt(gu2gv2) + sqrt(abs(gamma*gPr.dd(grho)));
  glambda = max(glambda(gauss.mapM), glambda(gauss.mapP));
  glambda.reshape(NGauss, Nfaces*K);
  glambda = outer(ones(NGauss), glambda.max_col_vals());
  glambda.reshape(NGauss*Nfaces, K);

  rhsQ(All,1) += CurvedDGJump2D(glambda.dm(grho),  gmapB, glambda.dm(brho) )/2.0;
  rhsQ(All,2) += CurvedDGJump2D(glambda.dm(grhou), gmapB, glambda.dm(brhou))/2.0;
  rhsQ(All,3) += CurvedDGJump2D(glambda.dm(grhov), gmapB, glambda.dm(brhov))/2.0;
  rhsQ(All,4) += CurvedDGJump2D(glambda.dm(gEner), gmapB, glambda.dm(bEner))/2.0;

  time_rhs += (timer.read() - trhs);
}

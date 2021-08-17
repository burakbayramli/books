// BuildCurvedOPS2D.m
// function [cinfo] = BuildCurvedOPS2D(intN)
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"


//---------------------------------------------------------
void NDG2D::BuildCurvedOPS2D(int intN)
//---------------------------------------------------------
{
  // function [cinfo] = BuildCurvedOPS2D(intN)
  // purpose: build curved info for curvilinear elements

  // Globals2D;
  DVec cR,cS,cW,  gz,gw, grf1,gsf1, x1,y1;
  DVec crx,csx,cry,csy,cJ, grx,gsx,gry,gsy,gJ,gsJ, gnx,gny;
  DMat cV,cVT,cDr,cDs, cMM;
  DMat gV[4], gVT[4], gVRR[4], gDr[4], gDs[4];
  DMat_Diag cJcW, crxD,cryD,csxD,csyD, gwgsJD;
  int Ncub=0, f1=0,f2=0,k1=0,k2=0,c=0;

  // 1. Create cubature information

  // 1.1 Extract cubature nodes and weights
  // [cR,cS,cW,Ncub] = Cubature2D(intN);
  Cubature2D(intN, cR,cS,cW,Ncub);

  // 1.1. Build interpolation matrix (nodes->cubature nodes)
  cV = InterpMatrix2D(cR, cS); cVT = trans(cV);

  // 1.2 Evaluate derivatives of Lagrange interpolants at cubature nodes
  // [cDr,cDs] = Dmatrices2D(N, cR, cS, V);
  ::Dmatrices2D(N, cR, cS, this->V, cDr,cDs);

  // 2. Create surface quadrature information

  // 2.1 Compute Gauss nodes and weights for 1D integrals
  JacobiGQ(0.0, 0.0, intN, gz, gw);

  // 2.2 Build Gauss nodes running counter-clockwise on element faces
  int Ng=gz.size(); DVec mones = -ones(Ng); DMat gR(Ng,3), gS(Ng,3);
  gR(All,1) = gz; gR(All,2) = -gz; gR(All,3) = mones;
  gS(All,1) = mones; gS(All,2) = gz; gS(All,3) = -gz;

  // 2.3 For each face 
  for (f1=1; f1<=Nfaces; ++f1) {

    grf1 = gR.get_col(f1); gsf1 = gS.get_col(f1);

    // 2.3.1 build nodes->Gauss quadrature interpolation and differentiation matrices
    gV[f1] = InterpMatrix2D(grf1, gsf1);

    // store transposed and reverse-row formats
    gVT[f1] = trans(gV[f1]);  gVRR[f1] = reverse_rows(gV[f1]);

    ::Dmatrices2D(N, grf1, gsf1, this->V, gDr[f1],gDs[f1]);
  }

  // 3. For each curved element, evaluate custom operator matrices
  int Ncurved = curved.size();

  // 3.1 Store custom information in array of Matlab structs
  m_cinfo.resize(Ncurved);

  for (c=1; c<=Ncurved; ++c) 
  {
    // sets shape of {gnx,gx,gny,gy}
    m_cinfo(c).resize(Ng,Nfaces);

    // find next curved element and the coordinates of its nodes
    k1=curved(c); x1=x(All,k1); y1=y(All,k1); m_cinfo(c).elmt=k1;

    // compute geometric factors
    ::GeometricFactors2D(x1,y1,cDr,cDs, crx,csx,cry,csy,cJ);

    // load diagonal matrices
    cJcW = cJ*cW; crxD=crx; cryD=cry; csxD=csx; csyD=csy;

    // build mass matrix
  //cMM = cVT* diag(cJ.*cW)*cV; cinfo(c).MM = cMM;
    cMM = cVT*cJcW*cV; m_cinfo(c).MM = cMM;

    // build physical derivative matrices
  //m_cinfo(c).Dx = cMM \ (cVT*diag(cW.*cJ)*(diag(crx)*cDr + diag(csx)*cDs));
    m_cinfo(c).Dx = cMM | (cVT*cJcW*(crxD*cDr + csxD*cDs));

  //m_cinfo(c).Dy = cMM \ (cVT*diag(cW.*cJ)*(diag(cry)*cDr + diag(csy)*cDs));
    m_cinfo(c).Dy = cMM | (cVT*cJcW*(cryD*cDr + csyD*cDs));

    // build individual lift matrices at each face
    for (f1=1; f1<=Nfaces; ++f1) {
      k2 = EToE(k1,f1); f2 = EToF(k1,f1);

      // compute geometric factors
      ::GeometricFactors2D(x1,y1,gDr[f1],gDs[f1], grx,gsx,gry,gsy,gJ);

      // compute normals and surface Jacobian at Gauss points on face f1
      if      (1==f1) { gnx = -gsx;     gny = -gsy;     }
      else if (2==f1) { gnx =  grx+gsx; gny =  gry+gsy; }
      else if (3==f1) { gnx = -grx;     gny = -gry;     }

      gsJ = sqrt( sqr(gnx) + sqr(gny) );
      gnx /= gsJ;  gny /= gsJ;  gsJ *= gJ;

      // store normals and coordinates at Gauss nodes
      m_cinfo(c).gnx(All,f1) = gnx;  m_cinfo(c).gx(All,f1) = gV[f1]*x1;
      m_cinfo(c).gny(All,f1) = gny;  m_cinfo(c).gy(All,f1) = gV[f1]*y1;

      // store Vandermondes for '-' and '+' traces
      m_cinfo(c).gVM[f1] = gV[f1];
    //m_cinfo(c).gVP[f1] = gV(end:-1:1,:,f2);
      m_cinfo(c).gVP[f1] = gVRR[f2];

      gwgsJD = gw*gsJ;  // diagonal operator

      // compute and store matrix to lift Gauss node data
    //m_cinfo(c).glift[f1] = cMM | (  trans(gV[f1]) * gwgsJD);
      m_cinfo(c).glift[f1] = cMM | (       gVT[f1]  * gwgsJD);
    }
  }
}


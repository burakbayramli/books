// GaussFaceMesh2D.m
// function  gauss = GaussFaceMesh2D(NGauss)
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"


//---------------------------------------------------------
Gauss2D& NDG2D::GaussFaceMesh2D(int NGauss)
//---------------------------------------------------------
{
  // function: gauss = GaussFaceMesh2D(NGauss)
  // purpose:  compute Gauss nodes for face term integration, and interpolation matrices 
  // Note: m_gauss is a member object of class NDG2D
  Gauss2D& gauss = m_gauss;

  // allocate storage for geometric data for each element
  gauss.resize(NGauss, K, Nfaces);

  JacobiGQ(0, 0, NGauss-1, gauss.z, gauss.w);
  DVec face1r =  gauss.z,      face2r = -gauss.z, face3r = -ones(NGauss);
  DVec face1s = -ones(NGauss), face2s =  gauss.z, face3s = -gauss.z;

  DMat V1 = Vandermonde2D(N, face1r, face1s);  gauss.finterp[1] = V1*invV;
  DMat V2 = Vandermonde2D(N, face2r, face2s);  gauss.finterp[2] = V2*invV;
  DMat V3 = Vandermonde2D(N, face3r, face3s);  gauss.finterp[3] = V3*invV;

  gauss.interp.concat_v(gauss.finterp[1], gauss.finterp[2], gauss.finterp[3]);
  // store transpose
  gauss.interpT = trans(gauss.interp);

  // correct dimensions of {mapM, mapP} set in resize()
  gauss.mapM.range(1, NGauss*Nfaces*K);
  gauss.mapP.range(1, NGauss*Nfaces*K);

  DMat dVMdr, dVMds;
  DVec xk1, yk1, grx,gsx,gry,gsy,gJ, gnx,gny,gsJ;
  int f1=0,f2=0, k1=0,k2=0; IVec mids1, mids2, gmM;

  for (f1=1; f1<=Nfaces; ++f1)
  {
    const DMat& VM = gauss.finterp[f1];
    dVMdr = VM*Dr;  dVMds = VM*Ds;
    Index1D ids1((f1-1)*NGauss+1, f1*NGauss);
    for (k1=1; k1<=K; ++k1) 
    {
      // calculate geometric factors at Gauss points
      xk1 = x.get_col(k1); yk1 = y.get_col(k1);
      ::GeometricFactors2D(xk1, yk1, dVMdr, dVMds, grx,gsx,gry,gsy,gJ);

      // compute normals at Gauss points
      if      (1==f1) { gnx = -gsx;      gny = -gsy;     }
      else if (2==f1) { gnx =  grx+gsx;  gny =  gry+gsy; }
      else if (3==f1) { gnx = -grx;      gny = -gry;     }

      gsJ = sqrt( sqr(gnx) + sqr(gny) );
      gnx /= gsJ;  gny /= gsJ;  gsJ *= gJ;

      gauss.nx(ids1,k1) = gnx; gauss.ny(ids1,k1) = gny; gauss.sJ(ids1,k1) = gsJ;
      gauss.rx(ids1,k1) = grx; gauss.ry(ids1,k1) = gry; gauss.J (ids1,k1) = gJ;
      gauss.sx(ids1,k1) = gsx; gauss.sy(ids1,k1) = gsy;

      k2 = EToE(k1,f1); f2 = EToF(k1,f1); 

      if (k1!=k2) {
        // update maps for non-boundary faces
        mids1.range(ids1.lo(), ids1.hi());       // step by +1
        mids2.range(f2*NGauss, (f2-1)*NGauss+1); // step by -1
        gauss.mapP(mids1,k1) = gauss.mapM(mids2,k2);
      } else {
        // update maps for boundary faces
        gmM = gauss.mapM(ids1,k1);
        gauss.mapP(ids1,k1)=gmM;  gauss.mapB.append(gmM); 
        // update BC maps
        switch (BCType(k1,f1)) {
        case BC_In:        gauss.mapI.append(gmM); break;
        case BC_Out:       gauss.mapO.append(gmM); break;
        case BC_Wall:      gauss.mapW.append(gmM); break;
        case BC_Cyl:       gauss.mapC.append(gmM); break;
        case BC_Dirichlet: gauss.mapD.append(gmM); break;
        case BC_Neuman:    gauss.mapN.append(gmM); break;
        case BC_Slip:      gauss.mapS.append(gmM); break;
        }
      }
    }
  }

  gauss.x = gauss.interp * this->x;
  gauss.y = gauss.interp * this->y;
  gauss.W = outer(concat(gauss.w,gauss.w,gauss.w), ones(K));
  gauss.W.mult_element(gauss.sJ);

  return gauss;
}

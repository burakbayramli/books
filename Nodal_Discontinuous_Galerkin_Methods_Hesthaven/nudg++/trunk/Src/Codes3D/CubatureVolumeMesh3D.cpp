// CubatureVolumeMesh3D.m
// function cub = CubatureVolumeMesh3D(Corder)
// 2007/10/10
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG3D.h"


//---------------------------------------------------------
void NDG3D::CubatureVolumeMesh3D(int Corder, Cub3D& cub)
//---------------------------------------------------------
{
  // function cub = CubatureVolumeMesh3D(Corder)
  // purpose: build cubature nodes, weights and geometric factors for all elements
  //
  // Note: m_cub is member of Globals3D

  // set up cubature nodes
  Cubature3D(Corder, cub);

  // evaluate generalized Vandermonde of Lagrange interpolant functions at cubature nodes
  InterpMatrix3D(cub);    // Note: stores cub.V and cub.VT

  // evaluate local derivatives of Lagrange interpolants at cubature nodes
  Dmatrices3D(this->N, cub);

  // evaluate the geometric factors at the cubature nodes
  GeometricFactors3D(cub);

  umTRC(1, "NDG3D::CubatureVolumeMesh3D -- only creating chol factors for curved elements\n");

  // build a custom mass matrix for each curved element
  int Ncurved = curved.size(), k=0;

  DMat mmk; DMat_Diag D; DVec d;
  cub.mmCHOL.resize(Np*Np, Ncurved);
  cub.mm    .resize(Np*Np, Ncurved);

  for (int m=1; m<=Ncurved; ++m) {
    k = curved(m);
    d=cub.J(All,k); d*=cub.w;
    D.diag(d);                        // weighted diagonal
    mmk = cub.VT * D * cub.V;         // mass matrix for element k
    cub.mm(All,m)     = mmk;          // store mass matrix
    cub.mmCHOL(All,m) = chol(mmk);    // store Cholesky factorization

    // cub.mm(:,:,k) = cub.VT* diag( cub.J(All,k).*cub.W)*cub.V;
  }

  // incorporate weights and Jacobian
  cub.W = outer(cub.w, ones(K));
  cub.W.mult_element(cub.J);

  // compute coordinates of cubature nodes
  cub.x = cub.V * this->x;
  cub.y = cub.V * this->y;
  cub.z = cub.V * this->z;
}

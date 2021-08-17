// CubatureVolumeMesh2D.m
// function cub = CubatureVolumeMesh2D(Corder)
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"


//---------------------------------------------------------
Cub2D& NDG2D::CubatureVolumeMesh2D(int Corder)
//---------------------------------------------------------
{
  // function cub = CubatureVolumeMesh2D(Corder)
  // purpose: build cubature nodes, weights and geometric factors for all elements
  //
  // Note: m_cub is member of Globals2D

  // set up cubature nodes
  Cubature2D(Corder, m_cub);

  // evaluate generalized Vandermonde of Lagrange interpolant functions at cubature nodes
  InterpMatrix2D(m_cub);

  // evaluate local derivatives of Lagrange interpolants at cubature nodes
  Dmatrices2D(this->N, m_cub);

  // evaluate the geometric factors at the cubature nodes
  GeometricFactors2D(m_cub);

  // custom mass matrix per element
  DMat mmk; DMat_Diag D; DVec d;
  m_cub.mmCHOL.resize(Np*Np, K);
  m_cub.mm    .resize(Np*Np, K);

  for (int k=1; k<=K; ++k) {
    d=m_cub.J(All,k); d*=m_cub.w; D.diag(d);  // weighted diagonal
    mmk = m_cub.VT * D * m_cub.V;     // mass matrix for element k
    m_cub.mm(All,k)     = mmk;        // store mass matrix
    m_cub.mmCHOL(All,k) = chol(mmk);  // store Cholesky factorization
  }

  // incorporate weights and Jacobian
  m_cub.W = outer(m_cub.w, ones(K));
  m_cub.W.mult_element(m_cub.J);

  // compute coordinates of cubature nodes
  m_cub.x = m_cub.V * this->x;
  m_cub.y = m_cub.V * this->y;

  return m_cub;
}

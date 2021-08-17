// JacobiGQ.m
// function [x,w] = JacobiGQ(alpha,beta,N);
// Note: sort eigensystem to match Matlab.
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "VecSort_Type.h"

//---------------------------------------------------------
void JacobiGQ
(
  double  alpha,  // [in]
  double  beta,   // [in]
  int     N,      // [in]
  DVec&   x,      // [out]
  DVec&   w,      // [out]
  bool    sort    // sort eigensys?
)
//---------------------------------------------------------
{
  // function [x,w] = JacobiGQ(alpha,beta,N)
  // Purpose: Compute the N'th order Gauss quadrature points, x, 
  //          and weights, w, associated with the Jacobi 
  //          polynomial, of type (alpha,beta) > -1 ( <> -0.5).

  if (0==N) { x(1)=(alpha-beta)/(alpha+beta+2.0); w(1)=2.0; return; }

  double ab1 = alpha+beta+1.0, a1=alpha+1.0, b1=beta+1.0;
  DMat J, Vr;


  //#######################################################
  // Note: this assembly differs from the Matlab script.
  //       - manual assembly of diagonals
  //       - sorting of LAPACK eigensystem
  //#######################################################

  DVec h1 = 2.0*range(0,N)+alpha+beta, d0(N+1), d1(N); int i=0;

  // main diagonal: diag(-1/2*(alpha^2-beta^2)./(h1+2)./h1)
  double fac = -0.5*(SQ(alpha)-SQ(beta));
  for (i=1; i<=N+1; ++i) { d0(i) = fac / (h1(i)+2.0) / h1(i); }
  
  // Handle division by zero
  const double eps = 1e-16;
  if (alpha+beta < 10*eps) { d0(1)=0.0; }

  // 1st upper diagonal: diag(2./(h1(1:N)+2).*sqrt((1:N).*((1:N)+alpha+beta) .* ((1:N)+alpha).*((1:N)+beta)./(h1(1:N)+1)./(h1(1:N)+3)),1);
  for (i=1; i<=N; ++i) { d1(i)=2.0/(h1(i)+2.0)*sqrt(i*(i+alpha+beta)*(i+alpha)*(i+beta)/(h1(i)+1)/(h1(i)+3.0)); }

  // Form symmetric matrix from recurrence.
  J.diag(d0); J.set_diag(1,d1);
  J += trans(J);    // J = J + J';

  // Compute quadrature by eigenvalue solve
  // [Vr,D] = eig(J); x = diag(D);
  eig(J, x, Vr);

  if (sort) {

    // Note: Matlab appears to sort results from eig() 
    // so that the eigenvalues are in ascending order. 
    // Here we sort the columns of eigenvector matrix
    // with the same permutation required to sort the 
    // eigenvalues into ascending order. Target: make
    // w=1st row of eigenvector matrix match Matlab's.

    DVecSort sx=x;  IVec idx;
    sx.makeIndex(idx);        // find sort permutation
    sx.sortFromIndexVec(idx); // permute eigenvalues
    Vr.sort_cols(idx);        // permute eigenvectors
    x = sx;                   // copy sorted evals to x
  }

  w = Vr.get_row(1); w.SQR(); 
  w *= pow(2.0,ab1) / (ab1)*gamma(a1)*gamma(b1)/gamma(ab1);

#if (0)
  dumpDVec(x, "x");
  dumpDVec(w, "w");
  dumpDMat(Vr, "Vr");
  dumpDMat(J, "J");
#endif

}

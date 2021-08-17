// JacobiP.m
// function [P] = JacobiP(x,alpha,beta,N);
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"


//---------------------------------------------------------
DVec& JacobiP(const DVec& x, double alpha, double beta, int N)
//---------------------------------------------------------
{
  // function [P] = JacobiP(x,alpha,beta,N)
  // Purpose: Evaluate Jacobi Polynomial of type (alpha,beta) > -1
  //          (alpha+beta <> -1) at points x for order N and
  //          returns P[1:length(x)]
  // Note   : They are normalized to be orthonormal.

  double aold=0.0, anew=0.0, bnew=0.0, h1=0.0;
  double gamma0=0.0, gamma1=0.0;
  double ab=alpha+beta, ab1=alpha+beta+1.0, a1=alpha+1.0, b1=beta+1.0;

  int Nc = x.size();
  DVec* P = new DVec(Nc, 0.0, OBJ_temp, "P");
  DMat PL(N+1, Nc, 0.0);  DVec prow, x_bnew;

  // Initial values P_0(x) and P_1(x)
  gamma0 = pow(2.0,ab1)/(ab1)*gamma(a1)*gamma(b1)/gamma(ab1);

  if (0==N) { (*P)   = 1.0/sqrt(gamma0);  return (*P);
  } else { PL(1,All) = 1.0/sqrt(gamma0); }

  gamma1 = (a1)*(b1)/(ab+3.0)*gamma0;
  prow = ((ab+2.0)*x/2.0 + (alpha-beta)/2.0) / sqrt(gamma1);

  if (1==N) { (*P)   = prow; return (*P);
  } else { PL.set_row(2,prow); }

  // Repeat value in recurrence.
  aold = 2.0/(2.0+ab)*sqrt((a1)*(b1)/(ab+3.0));

  // Forward recurrence using the symmetry of the recurrence.
  for (int i=1; i<=(N-1); ++i) {
    h1 = 2.0*i+ab;
    anew = 2.0/(h1+2.0)*sqrt((i+1)*(i+ab1)*(i+a1)*(i+b1)/(h1+1.0)/(h1+3.0));
    bnew = - (SQ(alpha)-SQ(beta))/h1/(h1+2.0);
    x_bnew = x-bnew;
    PL.set_row(i+2, 1.0/anew*( -aold*PL.get_row(i) + x_bnew.dm(PL.get_row(i+1))));
    aold =anew;
  }

  (*P) = PL.get_row(N+1);
  return (*P);
}

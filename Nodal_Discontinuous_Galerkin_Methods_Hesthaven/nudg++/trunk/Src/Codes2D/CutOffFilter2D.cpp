// CutOffFilter2D.m
// function [F] = CutOffFilter2D(Nc,frac)
// 2007/07/07
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"

//---------------------------------------------------------
DMat& NDG2D::CutOffFilter2D(int Nc, double frac)
//---------------------------------------------------------
{
  // function [F] = CutOffFilter2D(Nc,frac)
  // Purpose : Initialize 2D cut off filter matrix of order Norderin

  DMat_Diag filterdiag(Np, 1.0);

  // build cut-off filter
  int sk = 1;
  for (int i=0; i<=N; ++i) {
    for (int j=0; j<=(N-i); ++j) {
      if ((i+j) >= Nc) {
        filterdiag(sk) = frac;
      }
      ++sk;
    }
  }

  DMat* F = new DMat("Filter", OBJ_temp);
  (*F) = V * filterdiag * invV;
  return (*F);
}


//---------------------------------------------------------
void NDG2D::filter_Q(const DMat& filter, DMat& Qio)
//---------------------------------------------------------
{
  DMat qn; int Nf=Qio.num_cols();
  for (int n=1; n<=Nf; ++n) {
    qn.borrow(Np,K, Qio.pCol(n));   // wrap nth field
    Qio.set_col(n, filter*qn);      // store filtered data
  }
}

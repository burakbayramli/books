// Filter2D.m
// function [F] = Filter2D(Norder,Nc,sp)
// 2007/07/07
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"

//---------------------------------------------------------
DMat& NDG2D::Filter2D(int Norder, int Nc, double sp)
//---------------------------------------------------------
{
  // function [F] = Filter2D(Norder,sp)
  // Purpose : Initialize 2D filter matrix of order Norderin
  //           Order of exponential filter is sp and cutoff Nc

  DMat_Diag filterdiag((Norder+1)*(Norder+2)/2, 1.0);
  double alpha = -log(eps);

  // build exponential filter
  int sk = 1;
  for (int i=0; i<=Norder; ++i) {
    for (int j=0; j<=(Norder-i); ++j) {
      if ((i+j) >= Nc) {
        filterdiag(sk) = exp(-alpha * pow(double(i+j-Nc)/double(Norder-Nc), double(sp)));
      }
      ++sk;
    }
  }

  DMat* F = new DMat("Filter", OBJ_temp);
  (*F) = V * filterdiag * invV;
  return (*F);
}

// CutOffFilter3D.m
// function [F] = CutOffFilter3D(Nc,frac)
// 2007/07/07
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG3D.h"

//---------------------------------------------------------
DMat& NDG3D::CutOffFilter3D(int Nc, double frac)
//---------------------------------------------------------
{
  // function [F] = CutOffFilter3D(Nc,frac)
  // Purpose : Initialize 3D cut off filter matrix of order Norderin

  DMat_Diag filterdiag(Np, 1.0);

  // build cut-off filter
  int sk = 1;
  for (int i=0; i<=N; ++i) {
    for (int j=0; j<=(N-i); ++j) {
      for (int k=0; k<=(N-i-j); ++k) {
        if ((i+j+k) >= Nc) {
          filterdiag(sk) = frac;
        }
        ++sk;
      }
    }
  }

  DMat* F = new DMat("Filter", OBJ_temp);
  (*F) = V * filterdiag * invV;
  return (*F);
}

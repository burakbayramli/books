// Filter3D.cpp
// [F] = Filter3D_exp   (int Nc, double sp);
// [F] = Filter3D_cutoff(int Nc, double frac);
// 2007/07/11
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG3D.h"

//---------------------------------------------------------
DMat& NDG3D::Filter3D_exp(int Nc, double sp)
//---------------------------------------------------------
{
  // Initialize 3D filter matrix of order N
  // Order of exponential filter is sp and cutoff Nc

  DMat_Diag filterdiag(Np, 1.0);
//double alpha = -log(eps);
//double alpha = 17.0;
  double alpha =  8.0;
  
  if (Nc >= N ) {
    Nc = N-1;
  }

  // build exponential filter
  int sk = 1;
  for (int i=0; i<=N; ++i) {
    for (int j=0; j<=(N-i); ++j) {
      for (int k=0; k<=(N-i-j); ++k) {
        if ((i+j+k) >= Nc) {
          filterdiag(sk) = exp(-alpha * pow(double(i+j-Nc)/double(N-Nc), sp));
        }
        ++sk;
      }
    }
  }

dumpDVec(filterdiag, "filterdiag");

  DMat* F = new DMat("Filter", OBJ_temp);
  (*F) = V * filterdiag * invV;
  return (*F);
}


//---------------------------------------------------------
DMat& NDG3D::Filter3D_cutoff(int Nc, double frac)
//---------------------------------------------------------
{
  // Initialize 3D cut off filter matrix of order N

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

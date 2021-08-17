// EquiNodes3D.m
// function [X,Y,Z] = EquiNodes3D(N)
// 2007/06/11
//---------------------------------------------------------
#include "NDGLib_headers.h"


//---------------------------------------------------------
void EquiNodes3D(int N, DVec& X, DVec& Y, DVec& Z)
//---------------------------------------------------------
{
  // function [X,Y,Z] = EquiNodes3D(N)
  // Purpose: compute the equidistributed nodes on the 
  //          reference tetrahedron

  // total number of nodes
  int Np = (N+1)*(N+2)*(N+3)/6;  double dN = double(N);

  // create equidistributed nodes on equilateral triangle
  X.resize(Np); Y.resize(Np); Z.resize(Np); 

  int sk = 1;
  for (int n=1; n<=(N+1); ++n) {
    for (int m=1; m<=(N+2-n); ++m) {
      for (int q=1; q<=(N+3-n-m); ++q) {
        X(sk) = -1.0 + (q-1.0)*2.0/dN;
        Y(sk) = -1.0 + (m-1.0)*2.0/dN;
        Z(sk) = -1.0 + (n-1.0)*2.0/dN;
        ++sk;
      }
    }
  }
}

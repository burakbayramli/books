// Cubature3D.cpp
// function [cubR,cubS,cubT, cubW, Ncub] = Cubature3D(Corder)
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
//#include "CubatureData3D.h"


//---------------------------------------------------------
void Cubature3D(int Corder, Cub3D& cub)
//---------------------------------------------------------
{
  // function [cubR,cubS,cubT, cubW, Ncub] = Cubature3D(Corder)
  // Purpose: provide multidimensional quadrature (i.e. cubature) 
  //          rules to integrate up to Corder polynomials
  
  DVec gA,wA, gB,wB, gC,wC, cubA,cubB,cubC, cubW;

  int gNA=(int)ceil((Corder+1.0)/2.0);  JacobiGQ(0.0, 0.0, gNA-1, gA,wA);
  int gNB=(int)ceil((Corder+1.0)/2.0);  JacobiGQ(1.0, 0.0, gNB-1, gB,wB);
  int gNC=(int)ceil((Corder+1.0)/2.0);  JacobiGQ(2.0, 0.0, gNC-1, gC,wC);
  
  int Nval = gNA*gNB*gNC;
  cubA.resize(Nval); cubB.resize(Nval); cubC.resize(Nval); cubW.resize(Nval);

  cub.w.resize(Nval);

  int sk = 1;
  for (int a=1; a<=gNA; ++a) {
    for (int b=1; b<=gNB; ++b) {
      for (int c=1; c<=gNC; ++c) {
        cubA(sk) = gA(a);
        cubB(sk) = gB(b);
        cubC(sk) = gC(c);
        cub.w(sk) = 0.125*wA(a)*wB(b)*wC(c);
        ++sk;
      }
    }
  }

  cub.R = 0.25*(1.0+cubA).dm(1.0-cubB).dm(1.0-cubC) - 1.0;
  cub.S = 0.5 *(1.0+cubB).dm(1.0-cubC) - 1.0;
  cub.T = cubC;

  cub.Ncub = cub.w.size();
}


//---------------------------------------------------------
void Cubature3D(int Corder, DVec& r, DVec& s, DVec& t, DVec& w, int& Ncub)
//---------------------------------------------------------
{
  Cub3D cub; Cubature3D(Corder, cub);
  r = cub.R;
  s = cub.S;
  t = cub.T;

  w = cub.w;
  
  Ncub = cub.Ncub;
}

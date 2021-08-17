// Cubature2D.m
// function [cubR,cubS,cubW, Ncub] = Cubature2D(Corder)
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "CubatureData2D.h"

void Cub2D_data(int Cn, DMat& Cdata);

//---------------------------------------------------------
void Cubature2D(int Corder, Cub2D& cub)
//---------------------------------------------------------
{
  // function [cubR,cubS,cubW, Ncub] = Cubature2D(Corder)
  // Purpose: provide multidimensional quadrature (i.e. cubature) 
  //          rules to integrate up to Corder polynomials

  Corder = std::min(28, Corder);

  if (Corder<=28) {
    DMat RSW;  Cub2D_data(Corder, RSW);
    cub.r = RSW(All,1);
    cub.s = RSW(All,2);
    cub.w = RSW(All,3); 
    cub.Ncub = RSW.num_rows();
  } else {

    umMSG(1, "Cubature2D(%d): Corder > 28 not yet tested\n", Corder);

    DVec cuba,cubwa, cubb,cubwb; 
    DMat cubA, cubB, cubR, cubS, cubW, tA,tB;

    int cubNA=(int)ceil((Corder+1.0)/2.0);
    int cubNB=(int)ceil((Corder+1.0)/2.0);

    JacobiGQ(0.0, 0.0, cubNA-1,  cuba,cubwa);
    JacobiGQ(1.0, 0.0, cubNB-1,  cubb,cubwb);
    
    cubA = outer( ones(cubNB), cuba );
    cubB = outer( cubb, ones(cubNA) );

    tA = 1.0+cubA; tB = 1.0-cubB;
    cubR = 0.5 * tA.dm(tB) - 1.0;
    cubS = cubB;
    cubW = 0.5 * outer(cubwb, cubwa);

    cub.r = cubR;
    cub.s = cubS;
    cub.w = cubW;
    cub.Ncub = cub.r.size();
  }
}


//---------------------------------------------------------
void Cubature2D(int Corder, DVec& r, DVec& s, DVec& w, int& Ncub)
//---------------------------------------------------------
{
  Cub2D cub; Cubature2D(Corder, cub);
  r = cub.r; s = cub.s; w = cub.w;
  Ncub = cub.Ncub;
}


//---------------------------------------------------------
void Cub2D_data(int Cn, DMat& RSW)
//---------------------------------------------------------
{
  DVec Cd;

  //-------------------------------------
  // load data for nth-order cubature 
  //-------------------------------------

  switch (Cn) {
  //------------------ # -- data  ---------
  case  1:  Cd.copy(   3, cub2D_1); break;
  case  2:  Cd.copy(   9, cub2D_2); break;
  case  3:  Cd.copy(  18, cub2D_3); break;
  case  4:  Cd.copy(  18, cub2D_4); break;
  case  5:  Cd.copy(  21, cub2D_5); break;
  case  6:  Cd.copy(  36, cub2D_6); break;
  case  7:  Cd.copy(  45, cub2D_7); break;
  case  8:  Cd.copy(  48, cub2D_8); break;
  case  9:  Cd.copy(  57, cub2D_9); break;

  case 10:  Cd.copy(  75, cub2D_10); break;
  case 11:  Cd.copy(  84, cub2D_11); break;
  case 12:  Cd.copy( 108, cub2D_12); break;
  case 13:  Cd.copy( 120, cub2D_13); break;
  case 14:  Cd.copy( 138, cub2D_14); break;
  case 15:  Cd.copy( 162, cub2D_15); break;
  case 16:  Cd.copy( 174, cub2D_16); break;
  case 17:  Cd.copy( 198, cub2D_17); break;
  case 18:  Cd.copy( 219, cub2D_18); break;
  case 19:  Cd.copy( 246, cub2D_19); break;

  case 20:  Cd.copy( 255, cub2D_20); break;
  case 21:  Cd.copy( 279, cub2D_21); break;
  case 22:  Cd.copy( 300, cub2D_22); break;
  case 23:  Cd.copy( 318, cub2D_23); break;
  case 24:  Cd.copy( 354, cub2D_24); break;
  case 25:  Cd.copy( 378, cub2D_25); break;
  case 26:  Cd.copy( 414, cub2D_26); break;
  case 27:  Cd.copy( 435, cub2D_27); break;
  case 28:  Cd.copy( 675, cub2D_28); break;
  default:  
    umERROR("Cubature2D(%d)", 
            "Invalid order for 2D cubature.  Expected [1:28]", Cn); 
    break;
  }

  // Data in Cd is stored as {r,s,w} triples.
  // Load triples as columns, then transpose
  // to sort data into columns for {R,S,W}.

  int Nr = Cd.size()/3;
  RSW.load(3, Nr, Cd.data());
  RSW.transpose();
}

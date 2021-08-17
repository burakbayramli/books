// MaxwellCurved2D_RHS.m
// function [rhsHx, rhsHy, rhsEz] = MaxwellCurvedRHS2D(cinfo, Hx,Hy,Ez)
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "MaxwellCurved2D.h"

//---------------------------------------------------------
void MaxwellCurved2D::RHS()
//---------------------------------------------------------
{
  // function [rhsHx, rhsHy, rhsEz] = MaxwellCurvedRHS2D(cinfo, Hx,Hy,Ez)
  // Purpose  : Evaluate RHS flux in 2D Maxwell TM form 

  //---------------------------------------------
  // Call base class version to initialize rhs:
  //---------------------------------------------
  Maxwell2D::RHS();

  //---------------------------------------------
  // then correct residuals at each curved element
  //---------------------------------------------

  static DVec gdHx, gdHy, gdEz;
  static DVec ezk1,hxk1,hyk1, ezk2,hxk2,hyk2, gnx,gny;
  static DVec gndotdH, cfluxHx,cfluxHy,cfluxEz;

  int n=0,f1=0,f2=0,k1=0,k2=0;
  int Ncinfo = m_cinfo.size(), Ng=m_cinfo(1).gnx.size();
  // must set size before "map" operations can be used
  gdHx.resize(Ng); gdHy.resize(Ng); gdEz.resize(Ng);
  double t1 = timer.read();

  // correct residuals at each curved element
  for (n=1; n<=Ncinfo; ++n) {

    // for each curved element computed L2 derivatives via cubature
    const CInfo2D& cur = m_cinfo(n);
    k1 = cur.elmt; const DMat& cDx=cur.Dx, cDy=cur.Dy; 
    
    // pre-load (k1) columns as vectors
    ezk1.borrow(Np,Ez.pCol(k1));
    hxk1.borrow(Np,Hx.pCol(k1));
    hyk1.borrow(Np,Hy.pCol(k1));
    
    rhsHx(All,k1) = -cDy*ezk1;
    rhsHy(All,k1) =  cDx*ezk1;
    rhsEz(All,k1) =  cDx*hyk1 - cDy*hxk1;

    // for each face of each curved element use Gauss quadrature based lifts
    for (f1=1; f1<=Nfaces; ++f1) {

      k2 = EToE(k1,f1);
      
      // pre-load (k2) columns as vectors
      ezk2.borrow(Np,Ez.pCol(k2));
      hxk2.borrow(Np,Hx.pCol(k2));
      hyk2.borrow(Np,Hy.pCol(k2));

      gnx = cur.gnx(All,f1); gny = cur.gny(All,f1);
      const DMat& gVM=cur.gVM[f1], gVP=cur.gVP[f1], glift=cur.glift[f1];

      if (k1 != k2) {
        // compute difference of solution traces at Gauss nodes
        gdHx = gVM*hxk1       - gVP*hxk2;
        gdHy = gVM*hyk1       - gVP*hyk2;
        gdEz = gVM*ezk1       - gVP*ezk2;
      } else {
        // (k1==k2): correct jump at Gauss nodes on domain boundary faces
        gdHx = 0.0;
        gdHy = 0.0;
        gdEz = 2.0*gVM*ezk1;
      }

      // perform upwinding
      gndotdH =  gnx.dm(gdHx) + gny.dm(gdHy);
      cfluxHx =  gny.dm(gdEz) + gndotdH.dm(gnx) - gdHx;
      cfluxHy = -gnx.dm(gdEz) + gndotdH.dm(gny) - gdHy;
      cfluxEz = -gnx.dm(gdHy) + gny.dm(gdHx) - gdEz;

      // lift flux terms using Gauss based lift operator
      rhsHx(All,k1) += glift*cfluxHx/2.0;
      rhsHy(All,k1) += glift*cfluxHy/2.0;
      rhsEz(All,k1) += glift*cfluxEz/2.0;
    }
  }

  //---------------------------
  time_rhs_c += timer.read() - t1;
  //---------------------------
}

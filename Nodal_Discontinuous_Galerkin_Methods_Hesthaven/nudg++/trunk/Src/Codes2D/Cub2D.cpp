// Cub2D.cpp
// 2D Cubature data
// 2007/06/06
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "Cub2D.h"



//---------------------------------------------------------
Cub2D::Cub2D()
//---------------------------------------------------------
: Ncub(0), 
  r("cub.r"), s("cub.s"), w("cub.w"), W("cub.W"),
  V ("cub.V"), Dr("cub.Dr"), Ds("cub.Ds"), 
  VT("cub.V' "), DrT("cub.Dr' "), DsT("cub.Ds' "),
  x("cub.x"), y("cub.y"), rx("cub.rx"), sx("cub.sx"), 
  ry("cub.ry"), sy("cub.sy"), J("cub.J"),
  mm("cub.mm"), mmCHOL("cub.mmCHOL")
{}


//---------------------------------------------------------
Gauss2D::Gauss2D()
//---------------------------------------------------------
: NGauss(0), 
  z("gauss.z"), w("gauss.w"),
  x("gauss.x"), y("gauss.y"), W("gauss.W"),
  mapM("gauss.mapM"), mapP("gauss.mapP"),
  mapI("gauss.mapI"), mapO("gauss.mapO"), mapW("gauss.mapW"),
  mapC("gauss.mapC"), mapS("gauss.mapS"), mapD("gauss.mapD"),
  mapN("gauss.mapN"), mapB("gauss.mapB"),
  nx("gauss.nx"), ny("gauss.ny"), J("gauss.J"), sJ("gauss.sJ"), 
  rx("gauss.rx"), ry("gauss.ry"), sx("gauss.sx"), sy("gauss.sy"),
  interp("gauss.interp"), interpT("gauss.interp' ")
{}


//---------------------------------------------------------
void Gauss2D::resize(int Ng, int K, int Nfaces)
//---------------------------------------------------------
{
  NGauss = Ng;
  int Nr = NGauss*Nfaces;
  nx.resize(Nr,K); ny.resize(Nr,K); sJ.resize(Nr,K);
  rx.resize(Nr,K); ry.resize(Nr,K);  J.resize(Nr,K);
  sx.resize(Nr,K); sy.resize(Nr,K);

  // MUST set correct dimensions here!
  mapM.resize(NGauss*Nfaces, K);
  mapP.resize(NGauss*Nfaces, K);

  // but these are filled as required.
  mapI.resize(0); mapO.resize(0);
  mapW.resize(0); mapB.resize(0);
  mapS.resize(0); mapD.resize(0);
  mapN.resize(0); mapC.resize(0);
}

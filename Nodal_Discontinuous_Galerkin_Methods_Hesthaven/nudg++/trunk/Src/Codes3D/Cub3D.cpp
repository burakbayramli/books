// Cub3D.cpp
// 3D Cubature data
// 2007/09/23
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "Cub3D.h"


//---------------------------------------------------------
Cub3D::Cub3D()
//---------------------------------------------------------
: Ncub(0), 
  R("cub.R"), S("cub.S"), T("cub.T"), w("cub.w"), W("cub.W"),
  V ("cub.V"), Dr("cub.Dr"), Ds("cub.Ds"), Dt("cub.Dt"), 
  VT("cub.V' "), DrT("cub.Dr' "), DsT("cub.Ds' "), DtT("cub.Dt' "),
  x("cub.x"), y("cub.y"), z("cub.z"), 
  rx("cub.rx"), sx("cub.sx"), tx("cub.tx"), 
  ry("cub.ry"), sy("cub.sy"), ty("cub.ty"), 
  rz("cub.rz"), sz("cub.sz"), tz("cub.tz"), 
  J("cub.J"),
  mm("cub.mm"), mmCHOL("cub.mmCHOL")
{
}


//---------------------------------------------------------
Gauss3D::Gauss3D()
//---------------------------------------------------------
: NGauss(0), 
  //z("gauss.z"), w("gauss.w"),
  //x("gauss.x"), y("gauss.y"), W("gauss.W"),
  mapM("gauss.mapM"), mapP("gauss.mapP"),
  mapI("gauss.mapI"), mapO("gauss.mapO"), mapW("gauss.mapW"),
  mapC("gauss.mapC"), mapS("gauss.mapS"), mapD("gauss.mapD"),
  mapN("gauss.mapN"), mapB("gauss.mapB"),
  nx("gauss.nx"), ny("gauss.ny"), J("gauss.J"), sJ("gauss.sJ"), 
  rx("gauss.rx"), ry("gauss.ry"), sx("gauss.sx"), sy("gauss.sy"),
  interp("gauss.interp"), interpT("gauss.interp' ")
{
#if (1)
  umERROR("Gauss3D", "Nigel, implement Gauss3D");
#endif
}


//---------------------------------------------------------
void Gauss3D::resize(int Ng, int K, int Nfaces)
//---------------------------------------------------------
{
#if (1)

  umERROR("Gauss3D::resize", "Nigel, implement Gauss3D");

#else
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
#endif
}

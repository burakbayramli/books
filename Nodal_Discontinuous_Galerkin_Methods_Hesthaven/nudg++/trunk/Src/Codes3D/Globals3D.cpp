// Globals3D.cpp
//
// 2007/06/11
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "Globals3D.h"


//---------------------------------------------------------
Globals3D::Globals3D()
//---------------------------------------------------------
: 
  //-------------------------------------
  // initialize member data
  //-------------------------------------
  Np(0), Nfp(0), N(0), K(0), Nfaces(4), 
  NODETOL(1e-12),
  r("r"), s("s"), t("t"),
  Dr("Dr"), Ds("Ds"), Dt("Dt"), LIFT("LIFT"), 
  Drw("Drw"), Dsw("Dsw"), Dtw("Dtw"), MassMatrix("MassMatrix"), 
  Fx("Fx"), Fy("Fy"), Fz("Fz"), nx("nx"), ny("ny"), nz("nz"), Fscale("Fscale"), sJ("sJ"),
  vmapB("vmapB"), mapB("mapB"), Fmask("Fmask"), BCType("BCType"), saveBCType("saveBCType"),
   mapI( "mapI"),  mapO( "mapO"),  mapW( "mapW"),  mapF( "mapF"),  mapC( "mapC"),  mapS( "mapS"),  mapM( "mapM"),  mapP( "mapP"),  mapD( "mapD"),  mapN( "mapN"),
  vmapI("vmapI"), vmapO("vmapO"), vmapW("vmapW"), vmapF("vmapF"), vmapC("vmapC"), vmapS("vmapS"), vmapM("vmapM"), vmapP("vmapP"), vmapD("vmapD"), vmapN("vmapN"),
  rx("rx"), ry("ry"), rz("rz"), sx("sx"), sy("sy"), sz("sz"), tx("tx"), ty("ty"), tz("tz"), J("J"), 
  rk4a("rk4a"), rk4b("rk4b"), rk4c("rk4c"),
  EToE("EToE"), EToF("EToF"), EToV("EToV"), 
  V("V"), invV("invV"), VVT("VVT"),
  VX("VX"), VY("VY"), VZ("VZ"), x("x"), y("y"), z("z"),

  // +NBN: added
  materialVals("materialVals"), epsilon("epsilon"),
  tstep(0), Nsteps(0),
  dt(0.001), time(0.0), FinalTime(0.0), RKtime(0.0), 
  pi(M_PI), eps(2.2204e-16),  // NBN: Matlab @ AMD64
  Xmin(0.0), Xmax(0.0), Ymin(0.0), Ymax(0.0), Zmin(0.0), Zmax(0.0),
  Nv(0), Nmats(0), Nbcs(0), Nsd(0),
  bIs3D(false), bCoord3D(false), bElement3D(false)
  //-------------------------------------------------------
{
  init();
}


//---------------------------------------------------------
Globals3D::~Globals3D()
//---------------------------------------------------------
{}


//---------------------------------------------------------
void Globals3D::init()
//---------------------------------------------------------
{
  pi = 4.0*atan(1.0);   // set pi to machine precision
  eps = 2.2204e-16;     // match 32-bit Matlab on AMD64

  rk4a.resize(5);
  rk4b.resize(5);
  rk4c.resize(6);

  // Low storage Runge-Kutta coefficients
  rk4a(1) =              0.0;
  rk4a(2) =  -567301805773.0 / 1357537059087.0;
  rk4a(3) = -2404267990393.0 / 2016746695238.0;
  rk4a(4) = -3550918686646.0 / 2091501179385.0;
  rk4a(5) = -1275806237668.0 /  842570457699.0;

  rk4b(1) =  1432997174477.0 /  9575080441755.0;
  rk4b(2) =  5161836677717.0 / 13612068292357.0;
  rk4b(3) =  1720146321549.0 /  2090206949498.0;
  rk4b(4) =  3134564353537.0 /  4481467310338.0;
  rk4b(5) =  2277821191437.0 / 14882151754819.0;

  rk4c(1) =              0.0;
  rk4c(2) =  1432997174477.0 / 9575080441755.0;
  rk4c(3) =  2526269341429.0 / 6820363962896.0;
  rk4c(4) =  2006345519317.0 / 3224310063776.0;
  rk4c(5) =  2802321613138.0 / 2924317926251.0;
  rk4c(6) =              1.0;
}


//---------------------------------------------------------
void Globals3D::reset()
//---------------------------------------------------------
{}


//---------------------------------------------------------
void Globals3D::clear()
//---------------------------------------------------------
{}

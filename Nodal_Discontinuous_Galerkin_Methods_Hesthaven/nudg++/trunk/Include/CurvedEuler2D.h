// CurvedEuler2D.h
// Solves the 2D vacuum Maxwell's equations on TM form
// 2007/07/12
//---------------------------------------------------------
#ifndef NDG__CurvedEuler2D_H__INCLUDED
#define NDG__CurvedEuler2D_H__INCLUDED

#include "NDG2D.h"

//---------------------------------------------------------
class CurvedEuler2D : public NDG2D
//---------------------------------------------------------
{
public:
  CurvedEuler2D();
  virtual ~CurvedEuler2D();
  virtual void Driver();

protected:

  virtual void Run();

  virtual void Resize();        // resize system arrays
  virtual void Resize_cub();    // resize cubature arrays
  virtual void SetIC();
  virtual void SetStepSize();
  virtual void InitRun();

  virtual void Summary();
  virtual void Report(bool bForce=false);
  virtual void FinalReport();


  // http://www.codeproject.com/cpp/FastDelegate.asp

  // define pointers to functions
  typedef void (CurvedEuler2D::*fp_IC)(const DVec& xi, const DVec& yi, double ti, DMat& Qo);
  typedef void (CurvedEuler2D::*fp_ES)(const DVec& xi, const DVec& yi, double ti, DMat& Qo);
  typedef void (CurvedEuler2D::*fp_BC)(const DVec& xi, const DVec& yi, const DVec& nxi, const DVec& nyi, const IVec& tmapI, const IVec& tmapO, const IVec& tmapW, const IVec& tmapC, double ti, DMat& Qio);

  fp_IC InitialSolution;  // function pointer to initial solution
  fp_ES ExactSolution;    // function pointer to exact solution
  fp_BC BCSolution;       // function pointer to boundary conditions

  // functions for initial/exact solutions
  void IsentropicVortexIC2D(const DVec& xi, const DVec& yi, double ti, DMat& Qo);
  void ChannelIC2D         (const DVec& xi, const DVec& yi, double ti, DMat& Qo);
  void CouetteIC2D         (const DVec& xi, const DVec& yi, double ti, DMat& Qo);

  // functions for boundary solutions
  void IsentropicVortexBC2D(const DVec& xi, const DVec& yi, const DVec& nxi, const DVec& nyi, const IVec& tmapI, const IVec& tmapO, const IVec& tmapW, const IVec& tmapC, double ti, DMat& Qio);
  void ChannelBC2D         (const DVec& xi, const DVec& yi, const DVec& nxi, const DVec& nyi, const IVec& tmapI, const IVec& tmapO, const IVec& tmapW, const IVec& tmapC, double ti, DMat& Qio);
  void CouetteBC2D         (const DVec& xi, const DVec& yi, const DVec& nxi, const DVec& nyi, const IVec& tmapI, const IVec& tmapO, const IVec& tmapW, const IVec& tmapC, double ti, DMat& Qio);

  void MapGaussFaceData();
  void PreCalcBdryData();
  void RHS(DMat& Qin, double ti, fp_BC SolutionBC);

  void Fluxes(DMat& Qin, DMat& F, DMat& G);
  void Fluxes(DMat& Qin, double gamma, DMat& F, DMat& G, DVec& rho, DVec& u, DVec& v, DVec& p);

  void LF2D (const DMat& lnx, const DMat& lny, DMat& QM, DMat& QP, double gamma, DMat& flux);
  void Roe2D(const DMat& lnx, const DMat& lny, DMat& QM, DMat& QP, double gamma, DMat& flux);
  void HLL2D(const DMat& lnx, const DMat& lny, DMat& QM, DMat& QP, double gamma, DMat& flux);


protected:

  //-------------------------------------
  // member data
  //-------------------------------------

  // select simulation mode
  enum {
    eIsentropicVortex = 0,
    eChannelFlow      = 1,
    eCouetteFlow      = 2,
    eCylinderFlow     = 3,
    eBoxFlow          = 4,
    eForwardStep      = 5,
    eScramInlet       = 6
  };

  double gamma, gm1;
  DMat Q, Q1, Q2, Qbc, rhsQ, resQ;
  DMat QM, QP, flux; // nflux, 
  DMat cQ, cF, cG, gQ, gQM, gQP;
  DVec resid;

  // store pre-calculated constant boundary data
  IVec gmapB;       // concatenated boundary maps
  DVec gxB, gyB;    // {x,y} coords of boundary nodes
  DVec rhoB,rhouB,rhovB,EnerB;
};

#endif  // NDG__CurvedEuler2D_H__INCLUDED

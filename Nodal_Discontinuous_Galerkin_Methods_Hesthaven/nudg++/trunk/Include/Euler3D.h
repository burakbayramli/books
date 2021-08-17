// Euler3D.h
// 
// 2007/07/11
//---------------------------------------------------------
#ifndef NDG__Euler333D_H__INCLUDED
#define NDG__Euler333D_H__INCLUDED

#include "NDG3D.h"

//---------------------------------------------------------
class Euler3D : public NDG3D
//---------------------------------------------------------
{
public:
  Euler3D();
  virtual ~Euler3D();
  virtual void Driver();

protected:

  virtual void Run();

  virtual void Resize();        // resize system arrays
  virtual void SetIC();
  virtual void SetStepSize();
  virtual void InitRun();

  virtual void Summary();
  virtual void Report(bool bForce=false);
  virtual void FinalReport();

  // precalculate any constant BC data
  virtual void PrecalcBdryData();



  // http://www.codeproject.com/cpp/FastDelegate.asp

  // define pointers to functions
  typedef void (Euler3D::*fp_IC)(const DVec& xi, const DVec& yi, const DVec& zi, double ti, DMat& Qo);
  typedef void (Euler3D::*fp_ES)(const DVec& xi, const DVec& yi, const DVec& zi, double ti, DMat& Qo);
  typedef void (Euler3D::*fp_BC)(const DVec& xi,  const DVec& yi,  const DVec& zi, 
                                 const DVec& nxi, const DVec& nyi, const DVec& nzi, 
                                 const IVec& tmapI, const IVec& tmapO, const IVec& tmapW, const IVec& tmapC, 
                                 double ti, DMat& Qio);

  fp_IC InitialSolution;  // function pointer to initial solution
  fp_ES ExactSolution;    // function pointer to exact solution
  fp_BC BCSolution;       // function pointer to boundary conditions

  // functions for initial/exact solutions
  void IsentropicVortexIC3D(const DVec& xi, const DVec& yi, const DVec& zi, double ti, DMat& Qo);
  void CouetteIC3D         (const DVec& xi, const DVec& yi, const DVec& zi, double ti, DMat& Qo);

  // functions for boundary solutions
  void IsentropicVortexBC3D(const DVec& xi,  const DVec& yi,  const DVec& zi, 
                            const DVec& nxi, const DVec& nyi, const DVec& nzi, 
                            const IVec& tmapI, const IVec& tmapO, const IVec& tmapW, const IVec& tmapC, 
                            double ti, DMat& Qio);

  void CouetteBC3D(const DVec& xi,  const DVec& yi,  const DVec& zi, 
                   const DVec& nxi, const DVec& nyi, const DVec& nzi, 
                   const IVec& tmapI, const IVec& tmapO, const IVec& tmapW, const IVec& tmapC, 
                   double ti, DMat& Qio);


  void RHS(DMat& Qin, double ti, fp_BC SolutionBC);

  void Fluxes(DMat& Qin, DMat& F, DMat& G, DMat& H);
  void Fluxes(DMat& Qin, DMat& F, DMat& G, DMat& H, DVec& rho, DVec& u, DVec& v, DVec& w, DVec& p);


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
    eForwardStep      = 5
  };

  double gamma,gm1;

  DMat  Q, rhsQ, resQ;      // state data and residual
  DMat  cF,cG,cH;           // storage for flux data
  DMat  QM, QP, flux;       // 
  DMat  fM,gM,hM,fP,gP,hP;  //

  // store pre-calculated constant boundary data
  IVec gmapB;           // concatenated boundary maps
  DVec gxB,gyB,gzB;     // {x,y,z} coords of boundary nodes
  DVec rhoB,rhouB,rhovB,rhowB,EnerB;

};

#endif  // NDG__Euler333D_H__INCLUDED

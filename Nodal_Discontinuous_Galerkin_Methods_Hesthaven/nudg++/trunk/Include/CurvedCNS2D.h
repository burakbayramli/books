// CurvedCNS2D.h
// solver for 2D compressible Navier-Stokes equations
// 2007/07/12
//---------------------------------------------------------
#ifndef NDG__Curved_C_NS2D_H__INCLUDED
#define NDG__Curved_C_NS2D_H__INCLUDED

//#include "CurvedEuler2D.h"
#include "NDG2D.h"

//---------------------------------------------------------
class CurvedCNS2D : public NDG2D // CurvedEuler2D
//---------------------------------------------------------
{
public:
  CurvedCNS2D();
  virtual ~CurvedCNS2D();
  virtual void Driver();

protected:

  virtual void Run();

  virtual void Resize();            // resize system arrays
  virtual void Resize_cub();        // resize cubature arrays
  virtual void MapGaussFaceData();  // associate Gauss data
  virtual void PreCalcBdryData();   // precalc constant boundary data
  virtual void SetIC();
  virtual void SetStepSize();
  virtual void InitRun();

  virtual void Summary();
  virtual void Report(bool bForce=false);
  virtual void FinalReport();


  // define pointers to functions
  typedef void (CurvedCNS2D::*fp_IC)(const DVec& xi, const DVec& yi, double ti, DMat& Qo);
  typedef void (CurvedCNS2D::*fp_ES)(const DVec& xi, const DVec& yi, double ti, DMat& Qo);
  typedef void (CurvedCNS2D::*fp_BC)(const DVec& q1, const DVec& q2, const DVec& q3, const DVec& q4, 
                                     double rkti, DVec& b1, DVec& b2, DVec& b3, DVec& b4); 

  fp_IC InitialSolution;  // function pointer to initial solution
  fp_ES ExactSolution;    // function pointer to exact solution
  fp_BC BCSolution;       // function pointer to boundary conditions

  // functions for initial/exact solutions
  void CylIC2D      (const DVec& xi, const DVec& yi, double ti, DMat& Qo);
  void ChannelIC2D  (const DVec& xi, const DVec& yi, double ti, DMat& Qo);
  void BoxFlowIC2D  (const DVec& xi, const DVec& yi, double ti, DMat& Qo);
  void CouetteIC2D  (const DVec& xi, const DVec& yi, double ti, DMat& Qo);

  // functions for boundary solutions
  void CylBC2D      (const DVec& q1, const DVec& q2, const DVec& q3, const DVec& q4, double rkti, DVec& b1, DVec& b2, DVec& b3, DVec& b4);
  void ChannelBC2D  (const DVec& q1, const DVec& q2, const DVec& q3, const DVec& q4, double rkti, DVec& b1, DVec& b2, DVec& b3, DVec& b4);
  void BoxFlowBC2D  (const DVec& q1, const DVec& q2, const DVec& q3, const DVec& q4, double rkti, DVec& b1, DVec& b2, DVec& b3, DVec& b4);
  void CouetteBC2D  (const DVec& q1, const DVec& q2, const DVec& q3, const DVec& q4, double rkti, DVec& b1, DVec& b2, DVec& b3, DVec& b4);


  void RHS(double rkti, fp_BC SolutionBC);

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
    eBoxFlow          = 4
  };

  double gamma, gm1, mu, pbar, pref;
  DMat Q, rhsQ, resQ;
  
  // store pre-calculated constant boundary data
  IVec gmapB;       // concatenated boundary maps
  DVec gxB, gyB;    // {x,y} coords of boundary nodes
  DVec rhoB,rhouB,rhovB,EnerB;
};

#endif  // NDG__Curved_C_NS2D_H__INCLUDED

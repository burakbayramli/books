// CurvedINS2D.h
// 
// 2007/07/14
//---------------------------------------------------------
#ifndef NDG__CurvedINS2D_H__INCLUDED
#define NDG__CurvedINS2D_H__INCLUDED

#include "NDG2D.h"
#include "Mat_DIAG.h"

#ifdef NDG_USE_CHOLMOD
#include "CHOLMOD_solver.h"
#endif


//---------------------------------------------------------
class CurvedINS2D : public NDG2D
//---------------------------------------------------------
{
public:
  CurvedINS2D();
  virtual ~CurvedINS2D();
  virtual void Driver();

protected:

  virtual void Run();

  virtual void Resize();            // resize system arrays
  virtual void PreCalcBdryData();   // precalc constant boundary data
  virtual void SetIC();
  virtual void SetStepSize();
  virtual void InitRun();

  virtual void create_solvers();
  virtual void free_solvers();
  virtual void reset_solvers();
  virtual void TimeScaleBCData();

  virtual void Summary();
  virtual void Report(bool bForce=false);
  virtual void FinalReport();

  // http://www.codeproject.com/cpp/FastDelegate.asp

  // define pointers to functions
  typedef void (CurvedINS2D::*fp_ES)(const DVec& xin, const DVec& yin, double ti, double nu, DMat& Uxo, DMat& Uyo, DMat& PRo);
  typedef void (CurvedINS2D::*fp_BC)(const DVec& xin, const DVec& yin, const DVec& nxi, const DVec& nyi, 
                                     const IVec& MAPI, const IVec& MAPO, const IVec& MAPW, const IVec& MAPC, double ti, double nu,
                                     DVec& BCUX, DVec& BCUY, DVec& BCPR, DVec& BCDUNDT);

  fp_ES ExactSolution;    // func.ptr -> exact solution
  fp_BC ExactSolutionBC;  // func.ptr -> exact solution on boundary


  //-------------------------------------------------------
  // For each sim type, define pairs of functions that 
  // calculate both initial/exact and boundary solutions
  //-------------------------------------------------------

  // Channel:
  void INSchannelIC2D(const DVec& xin, const DVec& yin, double ti, double nu, DMat& Uxo, DMat& Uyo, DMat& PRo);
  void INSchannelBC2D(const DVec& xin, const DVec& yin, const DVec& nxi, const DVec& nyi, 
                       const IVec& MAPI, const IVec& MAPO, const IVec& MAPW, const IVec& MAPC, double ti, double nu,
                       DVec& BCUX, DVec& BCUY, DVec& BCPR, DVec& BCDUNDT);

  // Kovasznay:
  void KovasznayIC2D(const DVec& xin, const DVec& yin, double ti, double nu, DMat& Uxo, DMat& Uyo, DMat& PRo);
  void KovasznayBC2D(const DVec& xin, const DVec& yin, const DVec& nxi, const DVec& nyi, 
                       const IVec& MAPI, const IVec& MAPO, const IVec& MAPW, const IVec& MAPC, double ti, double nu,
                       DVec& BCUX, DVec& BCUY, DVec& BCPR, DVec& BCDUNDT);

  // VolkerCylinder:
  void INScylinderIC2D(const DVec& xin, const DVec& yin, double ti, double nu, DMat& Uxo, DMat& Uyo, DMat& PRo);
  void INScylinderBC2D(const DVec& xin, const DVec& yin, const DVec& nxi, const DVec& nyi, 
                       const IVec& MAPI, const IVec& MAPO, const IVec& MAPW, const IVec& MAPC, double ti, double nu,
                       DVec& BCUX, DVec& BCUY, DVec& BCPR, DVec& BCDUNDT);

  // PearsonVortex:
  void PearsonVortexIC2D(const DVec& xin, const DVec& yin, double ti, double nu, DMat& Uxo, DMat& Uyo, DMat& PRo);
  void PearsonVortexBC2D(const DVec& xin, const DVec& yin, const DVec& nxi, const DVec& nyi, 
                       const IVec& MAPI, const IVec& MAPO, const IVec& MAPW, const IVec& MAPC, double ti, double nu,
                       DVec& BCUX, DVec& BCUY, DVec& BCPR, DVec& BCDUNDT);

  // Stirer:
  void INSstirerIC2D(const DVec& xin, const DVec& yin, double ti, double nu, DMat& Uxo, DMat& Uyo, DMat& PRo) {}
  void INSstirerBC2D(const DVec& xin, const DVec& yin, const DVec& nxi, const DVec& nyi, 
                     const IVec& MAPI, const IVec& MAPO, const IVec& MAPW, const IVec& MAPC, double ti, double nu,
                     DVec& BCUX, DVec& BCUY, DVec& BCPR, DVec& BCDUNDT) {}

  // Taylor-Couette:
  //void INScouetteIC2D(const DVec& xin, const DVec& yin, double ti, double nu, DMat& Uxo, DMat& Uyo, DMat& PRo);
  //void INScouetteBC2D(const DVec& xin, const DVec& yin, const DVec& nxi, const DVec& nyi, 
  //                    const IVec& MAPI, const IVec& MAPO, const IVec& MAPW, const IVec& MAPC, double ti, double nu,
  //                    DVec& BCUX, DVec& BCUY, DVec& BCPR, DVec& BCDUNDT);

  // Backdrop:
  void BackdropIC2D(const DVec& xin, const DVec& yin, double ti, double nu, DMat& Uxo, DMat& Uyo, DMat& PRo);
  void BackdropBC2D(const DVec& xin, const DVec& yin, const DVec& nxi, const DVec& nyi, 
                    const IVec& MAPI, const IVec& MAPO, const IVec& MAPW, const IVec& MAPC, double ti, double nu,
                    DVec& BCUX, DVec& BCUY, DVec& BCPR, DVec& BCDUNDT);

  // Wedge:
  void WedgeIC2D(const DVec& xin, const DVec& yin, double ti, double nu, DMat& Uxo, DMat& Uyo, DMat& PRo);
  void WedgeBC2D(const DVec& xin, const DVec& yin, const DVec& nxi, const DVec& nyi, 
                    const IVec& MAPI, const IVec& MAPO, const IVec& MAPW, const IVec& MAPC, double ti, double nu,
                    DVec& BCUX, DVec& BCUY, DVec& BCPR, DVec& BCDUNDT);

  //-------------------------------------
  // translated m-files
  //-------------------------------------

  void CurvedINSPressureSetUp2D();
  void CurvedINSViscousSetUp2D();

  void INSAdvection2D();
  void INSPressure2D();

  void CurvedINSAdvection2D() {umERROR("CurvedINSAdvection2D", "TODO:");}
  void CurvedINSPressure2D()  {umERROR("CurvedINSPressure2D",  "TODO:");}
  void CurvedINSViscous2D();

  void INSLiftDrag2D(double ra);


protected:

  //-------------------------------------
  // member data
  //-------------------------------------

  // select simulation type
  enum {
    eNone           = 0,
    eChannel        = 1,
    eKovasznay      = 2,
    eStep           = 3,
    eVolkerCylinder = 4,
    ePearsonVortex  = 5,
    eStirer         = 6,
    eBackdrop       = 7,
    eWedge          = 8
  };

  double nu;                  // adjust Reynolds Number
  double g0, a0, a1, b0, b1;  // dual splitting scheme coefficients
  double tfac,tfac1,tpfac,tpfac1,tpfac2;
  
  DMat Ux, Uy, PR;
  DMat NUx, NUy, dpdn;
  DMat UxT,UyT, UxTT, UyTT;

  // storage for history of fields and nonlinear terms
  DMat Uxold,Uyold,NUxold,NUyold,dpdnold;
  DVec bcUx, bcUy, bcPR, bcdUndt;
  DVec Uxrhs,Uyrhs, rhsbcUx, rhsbcUy, rhsbcPR;
  DVec refrhsbcUx, refrhsbcUy, refrhsbcPR;
  DVec    refbcUx,    refbcUy,    refbcPR, refbcdUndt;
  // 
  IVec nbcmapD, vbcmapD;

  // store pre-calculated constant boundary data
  //IVec gmapB;       // concatenated boundary maps
  //DVec gxB, gyB;    // {x,y} coords of boundary nodes
  //DVec uB,vB,pB;    // {u,v,p} at boundary nodes


  // track various work times
  double time_setup, time_advection;
  double time_viscous, time_viscous_sol;
  double time_pressure, time_pressure_sol;


  //-------------------------------------
  // Select sparse Cholesky solver
  //-------------------------------------
#ifdef NDG_USE_CHOLMOD
  CHOLMOD_solver  *PRsystemC, *VELsystemC;
#else
  CS_Chol         *PRsystemC, *VELsystemC;
#endif

  // TODO: allow sparse LU solver for non-sym-pos-def
  // 

};

#endif  // NDG__CurvedINS2D_H__INCLUDED

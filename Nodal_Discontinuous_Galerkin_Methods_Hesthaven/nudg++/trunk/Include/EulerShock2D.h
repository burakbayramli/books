// EulerShock2D.h
// specialization of CurvedEuler2D for shocks
// 2007/08/15
//---------------------------------------------------------
#ifndef NDG__EulerShock2D_H__INCLUDED
#define NDG__EulerShock2D_H__INCLUDED

#include "CurvedEuler2D.h"


//---------------------------------------------------------
class EulerShock2D : public CurvedEuler2D
//---------------------------------------------------------
{
public:
  EulerShock2D();
  virtual ~EulerShock2D();
  virtual void Driver();

protected:

  virtual void Run();
  virtual void Run_Iter();

  virtual void Resize();        // resize system arrays
  virtual void SetIC();
  virtual void SetStepSize();
  virtual void InitRun();

  virtual void Summary();
  virtual void Report(bool bForce=false);
  virtual void FinalReport();

  void  precalc_limiter_data();
  DMat& EulerLimiter2D(const DMat& Qin, double ti);

  // functions for initial/exact solutions
  void ForwardStepIC2D(const DVec& xi, const DVec& yi, double ti, DMat& Qo);
  void InletIC2D      (const DVec& xi, const DVec& yi, double ti, DMat& Qo);

  // functions for boundary solutions
  void ForwardStepBC2D(const DVec& xi, const DVec& yi, const DVec& nxi, const DVec& nyi, const IVec& tmapI, const IVec& tmapO, const IVec& tmapW, const IVec& tmapC, double ti, DMat& Qio);
  void InletBC2D      (const DVec& xi, const DVec& yi, const DVec& nxi, const DVec& nyi, const IVec& tmapI, const IVec& tmapO, const IVec& tmapW, const IVec& tmapC, double ti, DMat& Qio);

  // adapt the mesh based on jumps in mach number
  void AdaptMesh_Mach(double jumptol);

protected:

  //-------------------------------------
  // member data
  //-------------------------------------

  DMat Q1, limQ, oldQ;
  
  // These members store geometric data precalculated 
  // in InitRun(), then reused in EulerLimiter2D()
  DMat Lim_dx, Lim_dy, Lim_fnx, Lim_fny, Lim_fL;
  DMat Lim_ctx, Lim_cty;
  DVec Lim_AVE, Lim_xv1,Lim_yv1, Lim_xv2,Lim_yv2, Lim_xv3,Lim_yv3;
  DVec xc0,xc1,xc2,xc3, yc0,yc1,yc2,yc3, A0,A1,A2,A3, A1_A2_A3;
  IVec Lim_E1,Lim_E2,Lim_E3, Lim_id1,Lim_id2,Lim_id3;
  IVec Lim_idI,Lim_idO,Lim_idW,Lim_idC, Lim_ids;

  bool bDoCalcIC;   // use IC routine for first iteration
  int  mesh_level;  // track adaptive iterations
};

#endif  // NDG__EulerShock2D_H__INCLUDED

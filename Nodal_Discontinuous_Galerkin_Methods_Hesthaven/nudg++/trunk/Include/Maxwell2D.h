// Maxwell2D.h
// Solves the 2D vacuum Maxwell's equations on TM form
// 2007/06/06
//---------------------------------------------------------
#ifndef NDG__Maxwell2D_H__INCLUDED
#define NDG__Maxwell2D_H__INCLUDED

#include "NDG2D.h"


//---------------------------------------------------------
class Maxwell2D : public NDG2D
//---------------------------------------------------------
{
public:
  Maxwell2D();
  virtual ~Maxwell2D();
  virtual void Driver();

protected:

  virtual void Run();
  virtual void RHS();

  virtual void Resize();
  virtual void SetIC();
  virtual void SetStepSize();
  virtual void InitRun();
  virtual void Summary();
  virtual void Report(bool bForce=false);
  virtual void FinalReport();

protected:

  //-------------------------------------
  // member data
  //-------------------------------------
  double alpha, mmode, nmode;

  DMat    Hx,    Hy,    Ez;
  DMat   dHx,   dHy,   dEz;
  DMat rhsHx, rhsHy, rhsEz;
  DMat resHx, resHy, resEz;
  DMat ndotdH, fluxHx, fluxHy, fluxEz;
  // local derivatives of fields
  DMat Ezx, Ezy, CuHz;

  DMat Ezinit;    // store initial conditions

  // stepsize calculation
  DVec rLGL, w;
};

#endif  // NDG__Maxwell2D_H__INCLUDED

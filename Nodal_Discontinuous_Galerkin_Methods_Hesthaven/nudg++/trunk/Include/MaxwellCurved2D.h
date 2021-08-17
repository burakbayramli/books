// MaxwellCurved2D.h
// 
// 2007/06/06
//---------------------------------------------------------
#ifndef NDG__MaxwellCurved2D_H__INCLUDED
#define NDG__MaxwellCurved2D_H__INCLUDED

#include "Maxwell2D.h"


//---------------------------------------------------------
class MaxwellCurved2D : public Maxwell2D
//---------------------------------------------------------
{
public:
  MaxwellCurved2D();
  virtual ~MaxwellCurved2D();
  virtual void Driver();

protected:

  virtual void Run() { Maxwell2D::Run(); }
  virtual void RHS();

  // override these base class members
  virtual void SetIC();
  virtual void InitRun();
  virtual void FinalReport();
  virtual double GetAnalyticError();

protected:

  //-------------------------------------
  // member data
  //-------------------------------------
  DVec m_alpha, m_theta, m_rad;
  double alpha0;

//DMat    Hx,    Hy,    Ez;
//DMat   dHx,   dHy,   dEz;
//DMat rhsHx, rhsHy, rhsEz;
//DMat resHx, resHy, resEz;
//DMat ndotdH, fluxHx, fluxHy, fluxEz;
  // local derivatives of fields
//DMat Ezx, Ezy, CuHz;

//DMat m_Ezinit;

  // stepsize calculation
  // DVec rLGL, w;
};

#endif  // NDG__MaxwellCurved2D_H__INCLUDED

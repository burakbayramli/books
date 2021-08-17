// Maxwell3D.h
// solves the 3D vacuum Maxwell's equations
// 2007/06/14
//---------------------------------------------------------
#ifndef NDG__Maxwell_333D_H__INCLUDED
#define NDG__Maxwell_333D_H__INCLUDED

#include "NDG3D.h"


//---------------------------------------------------------
class Maxwell3D : public NDG3D
//---------------------------------------------------------
{
public:
  Maxwell3D();
  virtual ~Maxwell3D();
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
  double alpha, mmode, nmode, xmode, ymode; 

  DMat     Hx,    Hy,    Hz,    Ex,    Ey,    Ez;
  DMat    dHx,   dHy,   dHz,   dEx,   dEy,   dEz;
  DMat  rhsHx, rhsHy, rhsHz, rhsEx, rhsEy, rhsEz;
  DMat  resHx, resHy, resHz, resEx, resEy, resEz;
  DMat fluxHx,fluxHy,fluxHz,fluxEx,fluxEy,fluxEz;

  // local spatial derivatives
  DMat curlHx, curlHy, curlHz, curlEx, curlEy, curlEz;
  DMat ndotdH, ndotdE;

  DMat Ezinit;    // store initial conditions
  DMat EzAnal;    // analytic solution
  DMat errEz;     // error estimate

  DVec sampleweights; // data for monitoring 
  int  sampletet;     // selected sample point

  DVec sampleEz;      // Ez(t) at sample point 
  DVec sampleT;       // time for each Ez(t)
};

#endif  // NDG__Maxwell_333D_H__INCLUDED

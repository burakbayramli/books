// ROHOP3D.h
// 
// 2007/10/05
//---------------------------------------------------------
#ifndef NDG__ROHOP3D_H__INCLUDED
#define NDG__ROHOP3D_H__INCLUDED

#include "NDG3D.h"
#include "FaceData3D.h"   // typedef "FaceMat"


//---------------------------------------------------------
class ROHOP3D : public NDG3D
//---------------------------------------------------------
{
public:
  ROHOP3D();
  virtual ~ROHOP3D();
  virtual void Driver();

protected:

  virtual void Run();

  DMat& RHS (const DMat& xi, const DMat& yi, const DMat& zi, 
             const DMat& xc, const DMat& yc, const DMat& zc);

  virtual void Resize();
  virtual void InitRun();
  virtual void Summary();
  virtual void Report(bool bForce=false);
  virtual void FinalReport();

  void ClearFaceData();
  void FindNonCon3D();
  void NonConPoissonIPDG3D(CSd& spOP, CSd& spMM);
  void BuildResultsTable();

  // exactu, exactdudx, exactdudy, exactdudz,   // function pointers
  DVec& IPDGErrorNorm3D(const DMat& uh, double tfac);

  DVec& ExactSol(const DMat& xi, const DMat& yi, const DMat& zi, 
                 const DMat& xc, const DMat& yc, const DMat& zc);

  // functions used by IPDGErrorNorm3D
  DVec& exactu   (const DVec& xi, const DVec& yi, const DVec& zi);
  DVec& exactdudx(const DVec& xi, const DVec& yi, const DVec& zi);
  DVec& exactdudy(const DVec& xi, const DVec& yi, const DVec& zi);
  DVec& exactdudz(const DVec& xi, const DVec& yi, const DVec& zi);

  // neighbors, ROHOPbcD3D, ROHOPbcN3D
  void  BdryTerms(const DMat& U, double tol, int NGauss, double tfac);

  // functions used by BdryTerms (was ROHOPterms3D)
  DVec& ubcD(const DVec& xi, const DVec& yi, const DVec& zi);
  DVec& ubcN(const DVec& xi, const DVec& yi, const DVec& zi);


protected:

  double  m_theta,m_delt,m_tfac;
  int     m_test, Nloop;

  DVec Ndof,AVEetaE1,AVEetaE2,AVEetaE3,AVEetaE4;
  DVec AVEoscT,AVEoscN,AVEoscD,AVEetaT,AVEoscTf;
  DVec sumerr,sumosc, M1T, M2T, M1E,M2E;

  IVec saveK;
  DVec saveMAXERROR;

  // typedef MatObj<FaceData3D*> FaceMat;
  FaceMat noncon;
  double  nc_tol;

  DMat  etaE, etaE1, etaE2, etaE3, etaE4;
  DMat  osc,  oscD,  oscN;


#if (0)

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

#endif
};

#endif  // NDG__ROHOP3D_H__INCLUDED

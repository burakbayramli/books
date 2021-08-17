// ARBCplane2D.h
// 
// 2007/08/27
//---------------------------------------------------------
#ifndef NDG__ARBCplane2D_H__INCLUDED
#define NDG__ARBCplane2D_H__INCLUDED

#include "NDG2D.h"
#include "OpStructs2D.h"  // {DGops2D, BCops2D}


//---------------------------------------------------------
class ARBCplane2D : public NDG2D
//---------------------------------------------------------
{
public:
  ARBCplane2D();
  virtual ~ARBCplane2D();
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

  void  BuildOperators();
  void  WriteSample();
  void  BuildDGops2D();
  void  BuildDG1D(double bcnx, double bcny, char coord, double normtol, DGops2D& bc);
  void  BuildBCops2D(int ne, int np, double  delt /* [bcmatrices,aj] */);

  DVec& DGgrad1D   (DGops2D& dg, DVec& Q, DVec& bcQ);
  DVec& DGpenalty1D(DGops2D& dg, DVec& Q, DVec& bcQ);

  // utility
  int CountBCFaces(int bc, const DMat& fnx, const DMat& fny, double bcnx, double bcny, double normtol);


  //-------------------------------------
  // member data
  //-------------------------------------

  // select simulation mode
  enum {
    eBar = 0,
  };

  double alpha, mmode, nmode;

  double delt;
  int ne, np;


  DMat    Ex,    Ey,    Hz;
  DMat resEx, resEy, resHz;
  DMat rhsEx, rhsEy, rhsHz;
  DMat   dEx,   dEy,   dHz;

  DMat    Qn,   Qe,   Qw,   Qs,   Qne,   Qse,   Qsw,   Qnw;
  DMat Qhatn,Qhate,Qhatw,Qhats,Qhatne,Qhatse,Qhatsw,Qhatnw;
  DMat resQn,resQe,resQw,resQs,resQne,resQse,resQsw,resQnw;
  DMat rhsQn,rhsQe,rhsQw,rhsQs,rhsQne,rhsQse,rhsQsw,rhsQnw;

  DVec sampEx, sampEy, sampHz;

  // stepsize calculation
  DVec rLGL, w;
  FILE* sampFile;
  DVec sampleweights; 
  int sampletri;

  enum {
     eNorth   = 0,
     eSouth   = 1,
     eEast    = 2,
     eWest    = 3,
     eNBCInfo = 4
  };

  // struct ARBC arbc;
  // struct DGBC dgbc;

  DGops2D   dgbc[4];
  BCops2D   arbc;

};

#endif  // NDG__ARBCplane2D_H__INCLUDED

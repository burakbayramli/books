// MaxwellNonCon2D.h
// 
// 2007/08/04
//---------------------------------------------------------
#ifndef NDG__MaxwellNonCon2D_H__INCLUDED
#define NDG__MaxwellNonCon2D_H__INCLUDED

#include "NDG2D.h"
#include "NonConInfo2D.h"


void Output_DG_tris(const PInfoV& pinfo);
void Output_DG_sol(const PInfoV& pinfo, const DMat& Q);

//---------------------------------------------------------
class MaxwellNonCon2D : public NDG2D
//---------------------------------------------------------
{
public:
  MaxwellNonCon2D();
  virtual ~MaxwellNonCon2D();
  virtual void Driver();

protected:

  virtual void Run();

  virtual void AdjustMesh_H();
  virtual void AdjustMesh_P();
  virtual void ShowMesh();

  virtual void RHS();       // select RHS_{H,P}
  virtual void RHS_base();  // TODO: move to base class
  virtual void RHS_H();     // non-conforming H
  virtual void RHS_P();     // non-conforming P

  virtual void Resize();
  virtual void SetIC();
  virtual void SetStepSize();
  virtual void InitRun();
  virtual void Summary();
  virtual void Report(bool bForce=false);
  virtual void FinalReport();

  // H non-con
  void BuildHNonCon2D(int NGauss, double tol, FInfoV& fInfo);

  // P non-con
  void BuildPNonCon2D(IVec& NList, int pK, DVec& pVX, DVec& pVY, IMat& pEToV, IMat& pBCType, PInfoV& pinfo);
  void BuildPNonCon2D(int  Norder, int pK, DVec& pVX, DVec& pVY, IMat& pEToV, IMat& pBCType, PInfoV& pinfo);


protected:

  // select simulation mode
  enum {
    eMode0 = 0,
    eModeH = 1,
    eModeP = 2
  };

  int noncon_mode;    // {H,P}
  int Nnoncon;        // # non-conforming faces
  int mesh_K;         // # elements read from mesh file

  //-------------------------------------
  // member data
  //-------------------------------------
  double alpha, mmode, nmode;

  // Since elements of each order require different numbers 
  // of DG nodes, we accumulate the node set by appending 
  // the {x,y} coords for the set of Nth order elements to 
  // the tail of a pair of parallel 1D arrays
  DVec xx,yy; // Hxx,Hyy,Ezz;

  DMat    Hx,    Hy,    Ez;
  DMat   dHx,   dHy,   dEz;
  DMat rhsHx, rhsHy, rhsEz;
  DMat resHx, resHy, resEz;
  DMat ndotdH, fluxHx, fluxHy, fluxEz;
  // local derivatives of fields
  DMat Ezx, Ezy, CuHz;

  IVec savemapB, savevmapB;
  double time_rhs_H, time_rhs_P;

  DMat Ezinit;    // store initial conditions

  // stepsize calculation
  DVec rLGL, w;

  // store information for N'th order elements 
  PInfoV    m_PInfo;
  int       max_pinf_id;  // total number of DG nodes
  int       Nmax;         // maximum order N

  // store information for non-conforming faces
  FInfoV    m_FInfo;
};

#endif  // NDG__MaxwellNonCon2D_H__INCLUDED

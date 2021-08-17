// Euler2D.h
// 
// 2007/03/05
//---------------------------------------------------------
#ifndef NDG__Euler2D_H__INCLUDED
#define NDG__Euler2D_H__INCLUDED

#include "NDG2D.h"

//---------------------------------------------------------
class Euler2D : public NDG2D
//---------------------------------------------------------
{
public:
  Euler2D();
  virtual ~Euler2D();
  virtual void Driver();

protected:

  virtual void Run();

  virtual void Resize();        // resize system arrays
  virtual void Resize_cub();    // resize cubature arrays
  virtual void SetIC();
  virtual void SetStepSize();
  virtual void InitRun();
  virtual void Report(bool bForce=false);
  virtual void DisplaySolution(bool bForce=false);


  // define pointers to functions
  typedef void (Euler2D::*fp_IC)(const DVec& xi, const DVec& yi, double ti, DMat& Qo);
  typedef void (Euler2D::*fp_ES)(const DVec& xi, const DVec& yi, double ti, DMat& Qo);
  typedef void (Euler2D::*fp_BC)(const DVec& xi, const DVec& yi, double ti, DMat& Qio); 

  fp_IC InitialSolution;  // function pointer to initial solution
  fp_ES ExactSolution;    // function pointer to exact solution
  fp_BC BCSolution;       // function pointer to boundary conditions

  // functions for initial/exact solutions
//void IsentropicVortex2D(const DVec& xi, const DVec& yi, double ti, DMat& Qo);
//void PlaneFlow2D       (const DVec& xi, const DVec& yi, double ti, DMat& Qo);
  void BumpFlowIC2D      (const DVec& xi, const DVec& yi, double ti, DMat& Qo);
  void ForwardStepIC2D   (const DVec& xi, const DVec& yi, double ti, DMat& Qo);
  void ShockBubbleIC2D   (const DVec& xi, const DVec& yi, double ti, DMat& Qo);

  // functions for boundary solutions
//void IsentropicVortexBC2D(const DVec& xi, const DVec& yi, double ti, DMat& Qio);
//void ChannelBC2D         (const DVec& xi, const DVec& yi, double ti, DMat& Qio);
  void BumpFlowBC2D        (const DVec& xi, const DVec& yi, double ti, DMat& Qio);
  void ForwardStepBC2D     (const DVec& xi, const DVec& yi, double ti, DMat& Qio);
  void ShockBubbleBC2D     (const DVec& xi, const DVec& yi, double ti, DMat& Qio);

  void MapGaussFaceData();
  void RHS(DMat& Qin, double ti, fp_BC SolutionBC);

  void Fluxes(DMat& Qin, DMat& F, DMat& G);
  void Fluxes(DMat& Qin, double gamma, DMat& F, DMat& G, DVec& rho, DVec& u, DVec& v, DVec& p);

  void LF2D   (const DVec& lnx, const DVec& lny, DMat& QM, DMat& QP, double gamma, DMat& FLUX);
  void Roe2D  (const DVec& lnx, const DVec& lny, DMat& QM, DMat& QP, double gamma, DMat& FLUX);
  void Eigen2D(const DVec& lnx, const DVec& lny, DMat& QM, DMat& QP, double gamma, DMat& FLUX, bool b=false);
  void HLLC2D (const DVec& lnx, const DVec& lny, DMat& QM, DMat& QP, double gamma, DMat& flux);

  void  EvalSmoothness();
  void  LimitWaves(DMat wave[5], const DMat& s);
  DMat& Limit2D(const DMat& Qin);

  void  Eval_SrcEXP();
  void  Eval_SrcIMP();
  void  Load_Aux();


protected:

  //-------------------------------------
  // member data
  //-------------------------------------

  // select simulation mode
  enum {
    eIsentropicVortex = 0,
    eChannelFlow      = 1,
    eBumpFlow         = 2,
    eForwardStep      = 3,
    eShockBubble      = 4
  };

  int sim_mode;
  double gamma,gamma1;

  DMat  m_Q, rhsQ, resQ;
  DMat  QsrcEXP,QsrcIMP;  // explicit/implicit source terms

  DMat  Q1, limQ;
  DMat  QM, QP, Qbc;
  DMat  cQ, cF,  cG;      // Q evaluated at cubature nodes
  DMat  gQ, gQM, gQP;     // Q, {Q+, Q-}, evaluated at gauss nodes
  DMat  flux;             // calculated flux
  DMat  aux;              // auxillary data (e.g. for geometric source terms)

  DVec  phi;              // smoothness indicator
  DVec  C1, C2, C3, C4;   // characteristic variables ("strengths")
                          // (coefficients in eigenvector sums)
  DMat  wave[5];          // waves using characteristic recomposition
  DMat  ws;               // wave speeds
  DMat  CP, CM, CR;       // coefficients at QP, QM and Roe average 

  DVec LimAVE;

  int m_nExplicit, m_nImplicit;

  // monitor range of primitive variables
  double rmin,rmax,umin,umax,vmin,vmax,pmin,pmax;

  // NBN: OPTIMIZE DEBUG
  double rhs_dt, rhs_time;
  double flux_time, flux_dt;

  // initial conditions routines
  // common/fsscorn/ xc0,yc0,xc1,yc1
  double xc0,yc0,xc1,yc1;
  double x0, y0, r0;

  double cellave(const DVec& xk, const DVec& yk);
  double fdisc(const double x, const double y);
  double fss(double s);
  double zeroin(const double x1, const double x2,double tol);

  void   RefineDisk(IMat& flags, double x0, double y0, double r0, double tol);

  DVec qin;     // initial state inside bubble
  DVec qout;    // initial state outside bubble

  double rhoin; // density in bubble:
  
  double rinf;  // density behind shock
  double vinf;  // velocity behind shock
  double einf;  // energy behind shock
  double pinf;  // pressure behind shock:

  double m_lambda;

};

#endif  // NDG__Euler2D_H__INCLUDED

// NDG2D.h
// interface implemented by 2D solvers
// 2008/08/16
//---------------------------------------------------------
#ifndef NDG__NDG_22D_H__INCLUDED
#define NDG__NDG_22D_H__INCLUDED

#include "Globals2D.h"
#include "Stopwatch.h"

// sparse matrix
#include "CS_Type.h"

// MatObj<FaceData> neighbors
// #include "MatObj_Type.h"
// #include "FaceData.h"       
//
// typedef MatObj<FaceData> FaceMat;


//---------------------------------------------------------
class NDG2D : public Globals2D
//---------------------------------------------------------
{
public:
  NDG2D();
  virtual ~NDG2D();
  virtual void Driver()=0;  // All simulators implement Driver()

  const char* GetClassName() const      { return class_name.c_str(); }
  const char* GetMeshFileName() const   { return FileName.c_str(); }
  bool Stationary() const { return m_bStationary; }
  bool HasAnalyticSol() const { return m_bHasAnalyticSol; }


protected:
  virtual void Run()=0;     // All simulators implement Run()
  virtual void InitRun();
  virtual void Summary();
  virtual void Report(bool bForce=false);
  virtual void FinalReport();
  virtual double GetAnalyticError();


  // Setup routines
  bool    StartUp2D();
  DMat&   Lift2D();
  void    Normals2D();
  void    BuildMaps2D();
  void    BuildBCMaps2D();
  void    BuildPeriodicMaps2D(double xperiod, double yperiod);
  void    MakeCylinder2D(const IMat& faces, double ra, double xo, double yo);
  void    CalcElemCentroids(DMat& centroid);

  bool    MeshReaderGambit2D(const string& fname);
  bool    load_BF_group(istream& is, char* buf);
  void    AdjustCylBC(double radius, double Cx, double Cy, int bc=BC_Cyl, bool toWall=false);

  void    Dmatrices2D();
  void    Dmatrices2D(int N, Cub2D& cub); // high-order cubature

  void    GeometricFactors2D();
  void    GeometricFactors2D(Cub2D& cub); // high-order cubature

  void    dtscale2D(DVec& dtscale);

  void    BuildCurvedOPS2D(int Nc);
  void    InterpMatrix2D(Cub2D& cub);
  DMat&   InterpMatrix2D(const DVec&, const DVec&);

  // cubature and quadrature routines
//Cub2D&    BuildCubatureMesh2D(int Corder);
  Cub2D&    CubatureVolumeMesh2D(int Corder);
  Gauss2D&  GaussFaceMesh2D(int NGauss);

  void PhysDmatrices2D(
    const DVec& x1,     // [in]
    const DVec& y1,     // [in]
    const DMat& interp, // [in]
          DMat& Dx,     // [out]
          DMat& Dy);    // [out]


  void CurvedDGGrad2D (
    const DMat& cU,       // [in]
    const DMat& gU,       // [in]
    const IVec& gmapD,    // [in]
    const DVec& bcU,      // [in]
          DMat& dUdx,     // [out]
          DMat& dUdy);    // [out]


  DMat& CurvedDGDiv2D (
    const DMat& cU,       // [in]
    const DMat& cV,       // [in]
    const DMat& gU,       // [in]
    const DMat& gV,       // [in]
    const IVec& gmapN,    // [in]
    const DVec& bcNdotU); // [in]


  DMat& CurvedDGJump2D (
      const DMat& gU,     // [in]
      const IVec& gmapD,  // [in]
      const DVec& bcU);   // [in]



/*
  void CurvedDGPenalty2D(
          Cub2D&   cub,   // [in]
          Gauss2D& gauss, // [in]
    const IVec& straight, // [in]
    const IVec& curved,   // [in]
    const DMat& cU,       // [in]
    const DMat& gU,       // [in]
    const IVec& gmapD,    // [in]
    const DVec& bcU,      // [in]
        DMat& penaltyU);  // [out]
*/

/*
  void CubatureVolumeMatrices2D(
    int   Corder,     // [in]
    CSd&  cphi,       // [out] sparse
    CSd&  cdphidx,    // [out] sparse
    CSd&  cdphidy,    // [out] sparse
    DMat_Diag& cw,    // [out] diag
    CSd&  cmminv,     // [out] sparse
    CSd&  cmm);       // [out] sparse
*/

  void CurvedPoissonIPDG2D(
    Gauss2D&  gauss,  // [in]
    Cub2D&    cub,    // [in]
    CSd&      spOP,   // [out] sparse
    CSd&      spMM);  // [out] sparse

  void CurvedPoissonIPDGbc2D(
    Gauss2D&  gauss,  // [in]
    CSd&      spOP);  // [out] sparse


  void Sample2D(double  xout,           // [in]
                double  yout,           // [in]
                DVec&   sampleweights,  // [out]
                int&    sampletri);     // [out]

  // FInfo*    neighbors;   // information for non-conforming faces
  // void BuildHNonCon2D(int NGauss, double tol, FInfo*& neighbors);

  void    FindLocalCoords2D(
               int k,   // [in]
    const DVec& xout,   // [in]
    const DVec& yout,   // [in]
          DVec& rOUT,   // [out]
          DVec& sOUT);  // [out]

#if (THIS_IS_READY)
  //#######################################################

  void    PartialLiftData2D(
    int k1, int f1,       // [in]
    int k2, int f2,       // [in]
    int     noncon,       // [in]
    const DVec&    xy1,   // [in]
    const DVec&    xy2,   // [in]
    DMat&     phiminus,   // [out]
    DMat& gradphiminus,   // [out]
    DMat&     phiplus,    // [out]
    DMat& gradphiplus);   // [out]


  void    PartialGaussData2D(
    int k1, int f1,   // [in]
    int k2, int f2,   // [in]
    double tol,       // [in]
    int noncon,       // [in]
    const DVec& xy1,  // [in]
    const DVec& xy2,  // [in]
    DVec&   weights,  // [out]
    DVec&   gnx,      // [out]
    DVec&   gny,      // [out]
    double& hinv);    // [out]


  void    GaussTraceMatrices2D(
    double tol,         // [in]
    int NGauss,         // [in]
    CSd&   gphiminus,   // [out]
    CSd&   gphiplus,    // [out]
    CSd&   gphiBC,      // [out]
    DMat_Diag& spgnx,   // [out]
    DMat_Diag& spgny,   // [out]
    DMat_Diag& spgw,    // [out]
    DMat_Diag& sphinv); // [out]

  //#######################################################
#endif // (THIS_IS_READY)


  //-------------------------------------
  // DG functions
  //-------------------------------------
  void Div2D(const DMat& u,     // [in]
             const DMat& v,     // [in]
                   DMat& divu); // [out]


  void Grad2D(const DMat& u,    // [in]
                    DMat& ux,   // [out]
                    DMat& uy);  // [out]

  void Curl2D(const DMat& ux,   // [in]
              const DMat& uy,   // [in]
                    DMat& vz);  // [out]

  void Curl2D(const DMat& ux,   // [in]
              const DMat& uy,   // [in]
              const DMat& uz,   // [in]
                    DMat& vx,   // [out]
                    DMat& vy,   // [out]
                    DMat& vz);  // [out]


  //-------------------------------------
  // Filters
  //-------------------------------------
  DMat&   Filter2D(int Norder, int Nc, double sp);
  DMat&   CutOffFilter2D(int Nc, double frac);
  void    filter_Q(const DMat& filter, DMat& Q);

  //-------------------------------------
  // Mesh adaptivity
  //-------------------------------------
  DMat& ConformingHrefine2D(IMat& edgerefineflag, const DMat& Qin);
  void            Hrefine2D(IVec& refineflag);


  //-------------------------------------
  // Output functions
  //-------------------------------------
  void Triangulation2D(int Np, IMat& alltri);

  // render selected solution fields
  void OutputVTK(const DMat& FData, int order, int zfield=0);

  void Output_DG_tris();    // render the DG elements
  void Output_Mesh();       // render the mesh

  // Utility routines
  void OutputSampleXYZ(int sample_N, DMat &newX, DMat &newY, DMat &newZ, 
                       const DMat &FData, DMat &newFData, int zfield=0);
  void OutputSampleELMT2D(int sample_N, IMat& ELMT);
  void OutputNodes(bool bFaceNodes=false);
  void OutputNodes_cub();
  void OutputNodes_gauss();

protected:

  //-------------------------------------
  // member data
  //-------------------------------------
  int     sim_type;         // select simulation mode
  int     flux_type;        // select flux type
  int     Nfields;          // number of solution fields
  Strings sol_names;        // names for solution fields
  string  class_name;       // identify without RTTI


  //-------------------------------------------------------
  // N[*] : number of unpaired data points on [*] boundary
  //-------------------------------------------------------
  int  Ninflow, Noutflow, Nwall, Nfar, Ncyl;
  int  Ndirichlet, Nneuman, Nslip;

  int m_Np_K;               // was m_Npts_Nel
  int m_Nfp_Nfaces_K;       // was m_Nfq_Nfaces_Nel
  int m_Nfp_Nfaces;         // was m_Nfq_Nfaces
  int m_Np_Nfields;         // was m_Npts_Nfields
  
  stopwatch timer;          // timer class
  string  FileName;         // gambit .neu-format mesh file
  double  m_eps;            // machine precision
  double  ti0,ti1,tw1,trhs; // time data
  double  time_rhs,         // time for evaluating RHS
          time_rhs_c,       // time to adjust for curved elements
          time_flux,        // time to evaluate flux
          time_upw,         // time to evaluate eigen-flux
          time_bc,          // time to evaluate BCs
          time_source,      // time to evaluate source terms
          time_iter,        // time for 1 iteration
          time_limit,       // time used by "limiters"
          time_work,        // total NDG++ time
          time_total;       // total simulation time

  int     Nreport;          // frequency of reporting
  int     Nrender;          // frequency of rendering
  int     Nplotfield;       // selected field to plot
  int     NvtkInterp;       // Vtk interpolation order
  DMat    Q_plot;           // storage for fields to render
  DVec    dtscale;          // stepsize calculation
  DMat    m_Filter;         // filter

  // boolean flags
  bool    m_bMenuLoaded;
  bool    m_bStationary;
  bool    m_bHasAnalyticSol;
  bool    m_bArgsSet;
  bool    m_bSummaryShown;
  bool    m_bHeaderShown;
  bool    m_bContinue;
  bool    m_bUserStop;
  bool    m_bDoTest;
  bool    m_bUseAMR;
  bool    m_bAdapted;
  bool    m_bApplyFilter;

  // iteritive h-refinement of default mesh
  int Nrefine, refine_count;

  // error analysis
  double  m_maxAbsError;
  DVec    m_ErrAnalytic;
  DVec    m_ErrEstimate;

  //-------------------------------------
  // static member data
  //-------------------------------------
  static int    N2Dobjects;     // count of active simulators 
  static int    PlotNumber;     // accumulated plot count
  static double TotalSimTime;   // accumulate time for multiple runs
};

extern NDG2D* g_D2;        // global pointer

#endif  // NDG__NDG_22D_H__INCLUDED

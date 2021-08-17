// NDG3D.h
// interface implemented by 3D solvers
// 2007/10/07
//---------------------------------------------------------
#ifndef NDG__NDG_333D_H__INCLUDED
#define NDG__NDG_333D_H__INCLUDED

#include "Globals3D.h"
#include "Stopwatch.h"

// sparse matrix
#include "CS_Type.h"


//---------------------------------------------------------
class NDG3D : public Globals3D
//---------------------------------------------------------
{
public:
  NDG3D();
  virtual ~NDG3D();
  virtual void Driver()=0;  // All simulators implement Driver()

  const char* GetClassName() const { return class_name.c_str(); }
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
  bool    StartUp3D();
  DMat&   Lift3D();
  void    Normals3D();
  void    BuildMaps3D();
  void    BuildBCMaps3D();
//void    MakeSphere3D(const IMat& faces, double ra, double xo, double yo, double zo);
  void    CalcElemCentroids(DMat& centroid);

  bool    MeshReaderGambit3D(const string& fname);
  bool    load_BF_group(istream& is, char* buf);

  void    Dmatrices3D();
  void    Dmatrices3D(int Nc, Cub3D& cub);  // high-order cubature

  void    GeometricFactors3D();
  void    GeometricFactors3D(Cub3D& cub);   // high-order cubature

  double  dtscale3D() const;


//void    BuildCurvedOPS3D(int Nc);
  void    InterpMatrix3D(Cub3D& cub);
  DMat&   InterpMatrix3D(const DVec&, const DVec&, const DVec&);

  // cubature and quadrature routines
//Cub2D&    BuildCubatureMesh3D(int Corder);
  void      CubatureVolumeMesh3D(int Corder, Cub3D& cub);
//Gauss2D&  GaussFaceMesh3D(int NGauss);


  void PhysDmatrices3D(
    const DVec& x1,     // [in]
    const DVec& y1,     // [in]
    const DVec& z1,     // [in]
    const DMat& interp, // [in]
          DMat& Dx,     // [out]
          DMat& Dy,     // [out]
          DMat& Dz);    // [out]
  
  void PartialLiftData3D(
    int k1, int f1,     // [in]
    int k2, int f2,     // [in]
    const DVec& xg,     // [in]
    const DVec& yg,     // [in]
    const DVec& zg,     // [in]
          DMat& VM,     // [out]
    DMat gradVM[4],     // [out]
          DMat& VP,     // [out]
    DMat gradVP[4]);    // [out]


  void    Div3D (const DMat& Ux, const DMat& Uy, const DMat& Uz, DMat& divU);
  void    Grad3D(const DMat& U, DMat& dUdx, DMat& dUdy, DMat& dUdz);
  void    Curl3D(const DMat& Ux, const DMat& Uy, const DMat& Uz, DMat& curlx, DMat& curly, DMat& curlz);


  void    PoissonIPDG3D  (CSd& spOP, CSd& spMM);
  DMat&   PoissonIPDGbc3D(DVec& ubc);

  void Sample3D(double  xout,           // [in]
                double  yout,           // [in]
                double  zout,           // [in]
                DVec&   sampleweights,  // [out]
                int&    sampletet);     // [out]


  void  FindLocalCoords3D (int k, const DVec& xi, const DVec& yi, const DVec& zi, DVec& rOUT, DVec& sOUT, DVec& tOUT);
  DMat& InterpNodeShapes3D(int k, const DVec& xi, const DVec& yi, const DVec& zi);


#if (0)
  //#######################################################
  // TODO:
  //#######################################################

//void    AdjustSphBC(double radius, double Cx, double Cy, double Cz, int bc=BC_Cyl);


  //#######################################################
  // TODO:
  //#######################################################
#endif

  //-------------------------------------
  // Filters
  //-------------------------------------
  DMat& Filter3D_exp   (int Nc, double sp);
  DMat& Filter3D_cutoff(int Nc, double frac);

  //-------------------------------------
  // Mesh adaptivity
  //-------------------------------------
//DMat& ConformingHrefine3D(IMat& edgerefineflag, const DMat& Qin);
  void            Hrefine3D(IVec& refineflag);


  //-------------------------------------
  // Output functions
  //-------------------------------------

  // render selected solution fields
  void OutputVTK(const DMat& FData, int order, int zfield=0);

  // TODO:
  // void Output_DG_tris();     // render the DG elements
  void Output_Mesh();           // render the mesh

  void OutputSampleXYZ(int sample_N, DMat &newX, DMat &newY, DMat &newZ, 
                       const DMat &FData, DMat &newFData, int zfield=0);
  void OutputSampleELMT3D(int sample_N, IMat& ELMT);
  void OutputNodes(bool bFaceNodes=false);

  void PlotContour3D(int nn, const DVec& field, const DVec& cntrs);


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
  static int    N3Dobjects;     // count of active simulators 
  static int    PlotNumber;     // accumulated plot count
  static double TotalSimTime;   // accumulate time for multiple runs
};

extern NDG3D* g_D3;        // global pointer

#endif  // NDG__NDG_333D_H__INCLUDED

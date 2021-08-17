// MaxwellNonCon2D.cpp
// member routines for class MaxwellNonCon2D
// 2007/08/04
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "MaxwellNonCon2D.h"


//---------------------------------------------------------
MaxwellNonCon2D::MaxwellNonCon2D()
//---------------------------------------------------------
{
  class_name = "MaxwellNonCon2D";

  m_PInfo.clear();    // P
  m_FInfo.clear();    // H
}


//---------------------------------------------------------
MaxwellNonCon2D::~MaxwellNonCon2D()
//---------------------------------------------------------
{
  // explicitly delete any "face info" objects
  if (m_FInfo.size()>0 && Nnoncon>0) 
  {
    // Note: using 1-based index into vector
    for (int n=1; n<=Nnoncon; ++n) {
      if (m_FInfo[n]) {
        // delete face info object
        delete m_FInfo[n];
        m_FInfo[n]=NULL;
      }
    }
    // clear vector of pointers
    m_FInfo.clear();
  }

  // explicitly delete any "P-info" objects
  if (m_PInfo.size() > 0) 
  {
    // TODO: select {0,1} as base
    for (size_t n=0; n<m_PInfo.size(); ++n) {
      if (m_PInfo[n]) {
        // delete face info object
        delete m_PInfo[n];
        m_PInfo[n]=NULL;
      }
    }
    // clear vector of pointers
    m_PInfo.clear();
  }
}


//---------------------------------------------------------
void MaxwellNonCon2D::Resize()
//---------------------------------------------------------
{
  // Allocate storage for solution and Runge-Kutta residual
  // Note: shape these arrays before use so that when 1D 
  // vectors are assigned to 2D matrices, they will be 
  // reshaped to the correct matrix dimensions.

  int Nr=0, Nc=0, Nrf=0;

  if (eModeH == noncon_mode) 
  {
    Nr=Np; Nc=K;  Nrf=Nfp*Nfaces;     // as (Np,K) matrices
  } 
  else  // eModeP
  { 
    Nr=max_pinf_id; Nc=1; Nrf=Nr;     // as (Nid,1) vectors
  } 

      Hx.resize(Nr,Nc);    Hy.resize(Nr,Nc);    Ez.resize(Nr,Nc);
   resHx.resize(Nr,Nc); resHy.resize(Nr,Nc); resEz.resize(Nr,Nc);
  Ezinit.resize(Nr,Nc);

  // Initialize storage for right hand side residuals
  rhsHx.resize(Nr,Nc); rhsHy.resize(Nr,Nc); rhsEz.resize(Nr,Nc);

  Q_plot.resize(Nr*Nc, 3);
}


//---------------------------------------------------------
void MaxwellNonCon2D::SetIC()
//---------------------------------------------------------
{
  // Set initial conditions for simulation
  // Ez = sin(mmode*pi*x) .* sin(nmode*pi*y);
  //
  // if ((eModeH == noncon_mode) || (eModeP== noncon_mode))

  mmode = 1.0; nmode = 1.0;


  if (eModeH == noncon_mode) 
  {
    // using {x,y} 2D matrices
    DVec tsinx =apply(sin, (mmode*pi*x)), tsiny=apply(sin, (nmode*pi*y));
    Ezinit = tsinx.dm(tsiny); Ez=Ezinit; Hx=0.0; Hy=0.0;
  } 
  else // eModeP
  {
    // using {xx,yy} 1D vectors in place of {x,y} 2D matrices
    DVec tsinx =apply(sin, (mmode*pi*xx)), tsiny=apply(sin, (nmode*pi*yy));
    Ez =tsinx.dm(tsiny); Hx =0.0; Hy =0.0;
  } 
}


//---------------------------------------------------------
void MaxwellNonCon2D::SetStepSize()
//---------------------------------------------------------
{
  if (eModeH == noncon_mode) 
  {
    // compute time step size (dt)

    JacobiGQ(0.0, 0.0, N, rLGL, w);
    double rmin = std::abs(rLGL(1)-rLGL(2));
    dtscale2D(dtscale); dt = dtscale.min_val()*rmin*(2.0/3.0);
  } 
  else if (eModeP == noncon_mode) 
  {
    // compute time step size (taking into 
    // account variable polynomial order)

    dt = 100.0; double fscalN=0.0;
    for (N=1; N<=Nmax; ++N) {
      if (m_PInfo[N]->K > 0) {
        fscalN = 0.5 * (m_PInfo[N]->Fscale).max_val();
        dt = std::min(dt, 2.0/(SQ(N)*fscalN));
      }
    }
  } 

  Nsteps = (int)ceil(FinalTime/dt);
  dt = FinalTime/(double)Nsteps;

#if (0)
  umLOG(1, "\n %s: N = %d,  dt = %8.6lf \n", 
           this->GetClassName(), N, dt);
#endif
}


//---------------------------------------------------------
void MaxwellNonCon2D::InitRun()
//---------------------------------------------------------
{

  if (eModeH == noncon_mode) {
    StartUp2D();      // construct grid and metric
    AdjustMesh_H();   // create non-conforming (H) elements
  } else {
    AdjustMesh_P();   // create non-conforming (H) elements
  }

  Resize();           // allocate arrays
  SetIC();            // set initial conditions
  SetStepSize();      // compute initial timestep (using IC's)

  //---------------------------------------------
  // base class version sets counters and flags
  //---------------------------------------------
  NDG2D::InitRun();
  
  time_rhs_H = 0.0;
  time_rhs_P = 0.0;

  //---------------------------------------------
  // Adjust reporting and render frequencies
  //---------------------------------------------
  Nreport = 100;        // set frequency of reporting (param)
  Nrender = Nreport;    // output frequency           (param)

  NvtkInterp =  8;      // set output resolution

  Summary();            // show simulation details
}


//---------------------------------------------------------
void MaxwellNonCon2D::Summary()
//---------------------------------------------------------
{
  //-------------------------------------
  // Summary of current simulator
  //-------------------------------------
  if (eModeH == noncon_mode) 
  {
    umLOG(1, "\nMaxwellNonCon2D: non-conforming (H) elements\n\n");
    umLOG(1,   "  Order (N)   : %d\n"
               "  Np          : %d\n"
               "  K           : %d\n", this->N, this->Np, this->K);
  }
  else
  {
    umLOG(1, "\nMaxwellNonCon2D: non-conforming (P) elements\n\n");
    umLOG(1, "   poly. order   elmts   \n");
    umLOG(1, "-------------------------\n");

    int total_K = 0;
    for (N=1; N<=Nmax; ++N) 
    {
      // report data for polynomial order N
      const PInfo& pinf = *(m_PInfo[N]);
      if (pinf.K>0) {
        total_K += pinf.K;
        umLOG(1,   "      N = %2d : %5d\n", N, pinf.K);
      }
    }
    umLOG(1, "-------------------------\n"
             "Tot. elements %6d\n"
             "-------------------------\n", total_K);
  }

  umLOG(1, "\n  mesh file   : %s\n"
             "  report freq : %d\n"
             "  render freq : %d\n", GetMeshFileName(), Nreport, Nrender);

  //-------------------------------------
  // time data
  //-------------------------------------
  umLOG(1, "  Finaltime   : %0.2g\n"
           "  time-step   : %0.5g  (inital dt)\n"
           "  num. steps  : %d\n\n\n", 
           this->FinalTime, this->dt, this->Nsteps);

  m_bSummaryShown = true;
}


//---------------------------------------------------------
void MaxwellNonCon2D::ShowMesh()
//---------------------------------------------------------
{
  //-------------------------------------
  // check node sets and connectivity
  //-------------------------------------

  if (eModeH == noncon_mode) 
  {
    OutputNodes(false);   // volume nodes
    OutputNodes(true);    // face nodes
  } 
  else 
  {
    // write elements of each polynomial order
    ::Output_DG_tris(m_PInfo);
  }
}


//---------------------------------------------------------
void MaxwellNonCon2D::Report(bool bForce)
//---------------------------------------------------------
{
  static bool header_shown = false;
  if (1 == tstep && ! header_shown) {
    // print header
    umLOG(1, "\n step    time     Ezmin    Ezmax\n"
             "----------------------------------\n");
    header_shown = true;
  }

  if (1 == tstep || !umMOD(tstep,Nreport) || bForce) 
  {
    if (eModeH == noncon_mode) {
      umLOG(1, "%5d  %7.3lf   %8.5lf %8.5lf\n", tstep, time, Ez.min_val(), Ez.max_val());
    } else {
      umLOG(1, "%5d  %7.3lf   %8.5lf %8.5lf\n", tstep, time, Ez.min_val(), Ez.max_val());
    }
  }

  //#####################################
  // skip field output for timing tests
  //#####################################
  //return;

  if (!umMOD(tstep,Nrender) || bForce) 
  {
    if (eModeH == noncon_mode) 
    {
      Q_plot.set_col(1, Hx);    // load plot data
      Q_plot.set_col(2, Hy);
      Q_plot.set_col(3, Ez);
      OutputVTK(Q_plot, NvtkInterp);
    } 
    else    // eModeP
    {
      Q_plot.set_col(1, Hx);
      Q_plot.set_col(2, Hy);
      Q_plot.set_col(3, Ez);
      Output_DG_sol(m_PInfo, Q_plot);
    }
  }
}


//---------------------------------------------------------
void MaxwellNonCon2D::FinalReport()
//---------------------------------------------------------
{
  // force report on final step
  this->Report(true);

  // exactEz = sin(mmode*pi*x).*sin(nmode*pi*y);
  // maxabserror = max(max(abs(Ez-exactEz)))

  // report work times (in seconds)
  if (eModeH == noncon_mode) {
    umLOG(1, "\n time for NDG work  : %12.2lf secs\n", time_work);
    umLOG(1,   " time for RHS base  : %12.2lf secs\n", time_rhs);
    umLOG(1,   " time for RHS H-non : %12.2lf secs\n", time_rhs_H);
    umLOG(1,   " time for main loop : %12.2lf secs\n\n", time_total);
  } else {
    umLOG(1, "\n time for NDG work  : %12.2lf secs\n", time_work);
    umLOG(1,   " time for RHS P-non : %12.2lf secs\n", time_rhs_P);
    umLOG(1,   " time for main loop : %12.2lf secs\n\n", time_total);
  }
}


//---------------------------------------------------------
void Output_DG_tris(const PInfoV& pinfo)
//---------------------------------------------------------
{
  int Nverts=0, Ncells=0, n=0,i=0,k=0;
  int Nmax = (int)pinfo.size() - 1;
  for (n=1; n<=Nmax; ++n) {
    if (pinfo[n]->K>0) {
      Nverts += pinfo[n]->x.size();
      Ncells += pinfo[n]->tri.num_rows();
    }
  }

  if (Nverts<3 || Ncells<1) {
    umMSG(1, "OutputDGTris: no data to plot\n");
    return;
  }

  // manage output file
  char fdataname[BUFSIZ];
  sprintf(fdataname, "dg_tri_%d.vtk", Ncells);
  FILE *fp = fopen(fdataname, "w");
  if (!fp) {
    umLOG(1, "Could no open %s for output!\n", fdataname);
    return;
  }

  //-------------------------------------
  // 1. Write the VTK header details
  //-------------------------------------
  fprintf(fp, "# vtk DataFile Version 2");
  fprintf(fp, "\nDG node connectivity");
  fprintf(fp, "\nASCII");
  fprintf(fp, "\nDATASET UNSTRUCTURED_GRID\n");
  fprintf(fp, "\nPOINTS %d double", Nverts);

  int offs=0; IVec offset(Nmax+1);

  //-------------------------------------
  // 2. Write the vertex data
  //-------------------------------------
  // trisurf(pinfo(N).tri, pinfo(N).x, pinfo(N).y, Ez(pinfo(N).ids));
  for (n=1; n<=Nmax; ++n) {
    if (pinfo[n]->K>0) {
      const DMat &X=pinfo[n]->x, &Y=pinfo[n]->y;
      int Nv = X.size();
      offs += Nv;  offset(n+1) = offs;
      for (i=1; i<=Nv; ++i) {
        fprintf(fp, "\n%20.12e  %20.12e  0.00", X(i), Y(i));
      }
    } else {
      // no elements at order N=n, so just update the offset array
      offset(n+1) = offset(n);
    }
  }


  //-------------------------------------
  // 3. Write the element connectivity
  //-------------------------------------

  // Number of indices required to define connectivity
  fprintf(fp, "\n\nCELLS %d %d", Ncells, 4*Ncells);

  int max_id=0;
  for (n=1; n<=Nmax; ++n) {
    if (pinfo[n]->K>0) {
      offs = offset(n);
      const IMat &tris=pinfo[n]->tri;
      int Nc = tris.num_rows();
      for (k=1; k<=Nc; ++k) {
        fprintf(fp, "\n3  %5d  %5d  %5d",
                tris(k,1)-1+offs,
                tris(k,2)-1+offs,
                tris(k,3)-1+offs);
      }
    }
  }


  //-------------------------------------
  // 4. Write the cell types
  //-------------------------------------

  // For each element (cell) write a single integer 
  // identifying the cell type.  The integer should 
  // correspond to the enumeration in the vtk file:
  // /VTK/Filtering/vtkCellType.h

  fprintf(fp, "\n\nCELL_TYPES %d\n", Ncells);
  for (int k=0; k<Ncells; ++k) {
    fprintf(fp, "5 ");            // 5:VTK_TRIANGLE
    if (! (k%10)) {
      fprintf(fp, "\n");
    }
  }

  //-------------------------------------
  // 5. Write the scalar "vtkPointData"
  //-------------------------------------
  // ...
  // ...
  // ...

  // add final newline to output
  fprintf(fp, "\n");
  fclose(fp);
}


//---------------------------------------------------------
void Output_DG_sol(const PInfoV& pinfo, const DMat& Q)
//---------------------------------------------------------
{
  static int cnt = 0;  ++cnt;
  int Nverts=0, Ncells=0, n=0,i=0,k=0;
  int Nmax = (int)pinfo.size() - 1;
  for (n=1; n<=Nmax; ++n) {
    if (pinfo[n]->K>0) {
      Nverts += pinfo[n]->x.size();
      Ncells += pinfo[n]->tri.num_rows();
    }
  }

  if (Nverts<3 || Ncells<1) {
    umMSG(1, "OutputDGTris: no data to plot\n");
    return;
  }

  // manage output file
  char fdataname[BUFSIZ];
//sprintf(fdataname, "dg_sol_%d_%04d.vtk", Ncells, cnt);
  sprintf(fdataname, "dg_sol_%04d.vtk", cnt);
  FILE *fp = fopen(fdataname, "w");
  if (!fp) {
    umLOG(1, "Could no open %s for output!\n", fdataname);
    return;
  }

  //-------------------------------------
  // 1. Write the VTK header details
  //-------------------------------------
  fprintf(fp, "# vtk DataFile Version 2");
  fprintf(fp, "\nDG node connectivity");
  fprintf(fp, "\nASCII");
  fprintf(fp, "\nDATASET UNSTRUCTURED_GRID\n");
  fprintf(fp, "\nPOINTS %d double", Nverts);

  int offs=0; IVec offset(Nmax+1);

  //-------------------------------------
  // 2. Write the vertex data
  //-------------------------------------
  // trisurf(pinfo(N).tri, pinfo(N).x, pinfo(N).y, Ez(pinfo(N).ids));
  for (n=1; n<=Nmax; ++n) {
    if (pinfo[n]->K>0) {
      const DMat &X=pinfo[n]->x, &Y=pinfo[n]->y;
      int Nv = X.size();
      offs += Nv;  offset(n+1) = offs;
      for (i=1; i<=Nv; ++i) {
        fprintf(fp, "\n%20.12e  %20.12e  0.00", X(i), Y(i));
      }
    } else {
      // no elements at order N=n, so just update the offset array
      offset(n+1) = offset(n);
    }
  }


  //-------------------------------------
  // 3. Write the element connectivity
  //-------------------------------------

  // Number of indices required to define connectivity
  fprintf(fp, "\n\nCELLS %d %d", Ncells, 4*Ncells);

  int max_id=0;
  for (n=1; n<=Nmax; ++n) {
    if (pinfo[n]->K>0) {
      offs = offset(n);
      const IMat &tris=pinfo[n]->tri;
      int Nc = tris.num_rows();
      for (k=1; k<=Nc; ++k) {
        fprintf(fp, "\n3  %5d  %5d  %5d",
                tris(k,1)-1+offs,
                tris(k,2)-1+offs,
                tris(k,3)-1+offs);
      }
    }
  }


  //-------------------------------------
  // 4. Write the cell types
  //-------------------------------------

  // For each element (cell) write a single integer 
  // identifying the cell type.  The integer should 
  // correspond to the enumeration in the vtk file:
  // /VTK/Filtering/vtkCellType.h

  fprintf(fp, "\n\nCELL_TYPES %d\n", Ncells);
  for (int k=0; k<Ncells; ++k) {
    fprintf(fp, "5 ");            // 5:VTK_TRIANGLE
    if (! (k%10)) {
      fprintf(fp, "\n");
    }
  }

  //-------------------------------------
  // 5. Write the scalar "vtkPointData"
  //-------------------------------------
  fprintf(fp, "\n\nPOINT_DATA %d", Nverts);
  
  int Nfields = Q.num_cols();

  for (int fld=1; fld<=Nfields; ++fld)
  {
    // For each field, write POINT DATA for each point
    // in the vtkUnstructuredGrid.  Note that if there 
    // are multiple sub-domains, then we must iterate 
    // over the set of domain objects, extracting part
    // of each field from each sub-domain.

    char buf[40]; sprintf(buf, "field%d", fld);
    fprintf(fp, "\nSCALARS %s double 1", buf);
    fprintf(fp, "\nLOOKUP_TABLE default");
    for (int n=1; n<=Q.num_rows(); ++n) {
      fprintf(fp, "\n%20.12e ", Q(n, fld));
    }
  }


  // add final newline to output
  fprintf(fp, "\n");
  fclose(fp);
}

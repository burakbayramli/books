// NDG2D_Output.cpp
// write selected fields to file
// 2007/10/03
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"


//---------------------------------------------------------
// Utility routines
//---------------------------------------------------------

// find number of triangles per element
int OutputSampleNelmt2D(int sample_N) 
{
  return (sample_N)*(sample_N+1)/2 + (sample_N-1)*(sample_N)/2;
}

// find number of nodes per face
int OutputSampleNpts2D(int sample_N) 
{
  return (sample_N+1) * (sample_N+2) / 2;
}

// load {R,S} output nodes
void OutputSampleNodes2D(int sample_N, DVec &R, DVec &S)
{
  int Npts = OutputSampleNpts2D(sample_N);
  R.resize(Npts); S.resize(Npts);
  double denom = (double)(sample_N);
  int sampleNq = sample_N+1;
  for (int sk=0, i=0; i<sampleNq; ++i) {
    for (int j=0; j<sampleNq-i; ++j, ++sk) {
      R[sk] = -1. + (2.*j)/denom;
      S[sk] = -1. + (2.*i)/denom;
    }
  }
}


//---------------------------------------------------------
void NDG2D::OutputVTK(const DMat& FData, int order, int zfield)
//---------------------------------------------------------
{
  static int count = 0;
  string output_dir = ".";

  // The caller loads each field of interest into FData, 
  // storing (Np*K) scalars per column.
  //
  // For high (or low) resolution output, the user can 
  // specify an arbitrary order of interpolation for 
  // exporting the fields.  Thus while a simulation may 
  // use N=3, we can export the solution fields with 
  // high-order, regularized elements (e.g. with N=12).

  string buf = umOFORM("%s/sim_N%02d_%04d.vtk", output_dir.c_str(), order, ++count);
  FILE *fp = fopen(buf.c_str(), "w");
  if (!fp) {
    umLOG(1, "Could no open %s for output!\n", buf.c_str());
    return;
  }

  // Set flags and totals
  int Output_N = std::max(2, order);
  int Ncells=0, Npts=0;

  Ncells = OutputSampleNelmt2D(Output_N);
  Npts   = OutputSampleNpts2D (Output_N);

  // set totals for Vtk output
  int vtkTotalPoints = this->K * Npts;
  int vtkTotalCells  = this->K * Ncells;
  int vtkTotalConns  = (this->EToV.num_cols()+1) * this->K * Ncells;


  //-------------------------------------
  // 1. Write the VTK header details
  //-------------------------------------
  fprintf(fp, "# vtk DataFile Version 2");
  fprintf(fp, "\nNuDG++ 2D simulation");
  fprintf(fp, "\nASCII");
  fprintf(fp, "\nDATASET UNSTRUCTURED_GRID\n");
  fprintf(fp, "\nPOINTS %d double", vtkTotalPoints);

  int newNpts=0;

  //-------------------------------------
  // 2. Write the vertex data
  //-------------------------------------

  DMat newX, newY, newZ, newFData;

  // Build new {X,Y,Z} vertices that regularize the 
  // elements, then interpolate solution fields onto 
  // this new set of elements:
  OutputSampleXYZ(Output_N, newX, newY, newZ, FData, newFData, zfield);
//double maxF1 = newFData.max_col_val_abs(1), scaleF=1.0;
//if (maxF1 != 0.0) { scaleF = 1.0/maxF1; }

  newNpts = newX.num_rows();

  if (zfield>0)
  {
    // write 2D vertex data, with z-elevation
    for (int k=1; k<=this->K; ++k) {
      for (int n=1; n<=newNpts; ++n) {
        // use exponential format to allow for
        // arbitrary (astro, nano) magnitudes:
        fprintf(fp, "\n%20.12e %20.12e %20.12e", 
                      newX(n, k), newY(n, k), newZ(n,k)); //*scaleF);
      }
    }
  } else {
    // write 2D vertex data to file
    for (int k=1; k<=this->K; ++k) {
      for (int n=1; n<=newNpts; ++n) {
        // use exponential format to allow for
        // arbitrary (astro, nano) magnitudes:
        fprintf(fp, "\n%20.12e %20.12e  0.0", 
                      newX(n, k), newY(n, k));
      }
    }
  }


  //-------------------------------------
  // 3. Write the element connectivity
  //-------------------------------------
  IMat newELMT;

  // Number of indices required to define connectivity
  fprintf(fp, "\n\nCELLS %d %d", vtkTotalCells, vtkTotalConns);

  // build regularized tri elements at selected order
  OutputSampleELMT2D(Output_N, newELMT);
  newNpts = OutputSampleNpts2D(Output_N);

  int newNTri = newELMT.num_rows();
  int newNVert = newELMT.num_cols();

  // write element connectivity to file
  for (int k=0; k<this->K; ++k) {
    int nodesk = k*newNpts;
    for (int n=1; n<=newNTri; ++n) {
      fprintf(fp, "\n%d", newNVert);
      for (int i=1; i<=newNVert; ++i) {
        fprintf(fp, " %5d", nodesk+newELMT(n, i));
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

  fprintf(fp, "\n\nCELL_TYPES %d", vtkTotalCells);

  for (int k=0; k<this->K; ++k) {
    fprintf(fp, "\n");
    for (int i=1; i<=Ncells; ++i) {
      fprintf(fp, "5 ");            // 5:VTK_TRIANGLE
      if (! (i%10))
        fprintf(fp, "\n");
    }
  }
  
  //-------------------------------------
  // 5. Write the scalar "vtkPointData"
  //-------------------------------------

  fprintf(fp, "\n\nPOINT_DATA %d", vtkTotalPoints);

  // For each field, write POINT DATA for each point 
  // in the vtkUnstructuredGrid. 

  int Nfields = FData.num_cols();
  for (int fld=1; fld<=Nfields; ++fld)
  {
    fprintf(fp, "\nSCALARS field%d double 1", fld);
    fprintf(fp, "\nLOOKUP_TABLE default");

    // Write the scalar data, using exponential format 
    // to allow for arbitrary (astro, nano) magnitudes:
    for (int n=1; n<=newFData.num_rows(); ++n) {
      fprintf(fp, "\n%20.12e ", newFData(n, fld));
    }
  }

  // add final newline to output
  fprintf(fp, "\n");
  fclose(fp);
}


//---------------------------------------------------------
void NDG2D::Output_DG_tris()
//---------------------------------------------------------
{
  static int count = 0;
  string output_dir = ".";

  // Write the current set of DG elements (triangles)
  // in vtk format.  Very similar to OutputVTK(), but
  // here we write the actual traingles used by the DG 
  // solver without regularization.  Use to record the 
  // mesh state, e.g. during adaptive mesh refinement.

  string buf = umOFORM("%s/mesh_N%02d_%04d.vtk", output_dir.c_str(), this->N, ++count);
  FILE *fp = fopen(buf.c_str(), "w");
  if (!fp) {
    umLOG(1, "Could no open %s for output!\n", buf.c_str());
    return;
  }

  // Set flags and totals
  int Output_N = this->N;
  int Ncells=0, Npts=0;

  Ncells = OutputSampleNelmt2D(Output_N);
  Npts   = OutputSampleNpts2D (Output_N);

  // set totals for Vtk output
  int vtkTotalPoints = this->K * Npts;
  int vtkTotalCells  = this->K * Ncells;
//int vtkTotalConns  = (this->ElmtToNode.num_cols()+1) * um->Nel * Ncells;
  int vtkTotalConns  = (this->EToV.num_cols()+1) * this->K * Ncells;


  //-------------------------------------
  // 1. Write the VTK header details
  //-------------------------------------
  fprintf(fp, "# vtk DataFile Version 2");
  fprintf(fp, "\nNuDG++ mesh");
  fprintf(fp, "\nASCII");
  fprintf(fp, "\nDATASET UNSTRUCTURED_GRID\n");
  fprintf(fp, "\nPOINTS %d double", vtkTotalPoints);

  int newNpts=0;

  //-------------------------------------
  // 2. Write the vertex data
  //-------------------------------------

  // write 2D vertex data to file
  for (int k=1; k<=this->K; ++k) {
    for (int n=1; n<=this->Np; ++n) {
      // use exponential format to allow for
      // arbitrary (astro, nano) magnitudes:
      fprintf(fp, "\n%20.12e %20.12e  0.0", 
                    x(n, k), y(n, k));
    }
  }

  //-------------------------------------
  // 3. Write the element connectivity
  //-------------------------------------
  IMat newELMT;

  // Number of indices required to define connectivity
  fprintf(fp, "\n\nCELLS %d %d", vtkTotalCells, vtkTotalConns);

  // build connectivity for tri elements at order N
  OutputSampleELMT2D(Output_N, newELMT);

  int newNTri = newELMT.num_rows();
  int newNVert = newELMT.num_cols();

  // write element connectivity to file
  for (int k=0; k<this->K; ++k) {
    int nodesk = k*Np;
    for (int n=1; n<=newNTri; ++n) {
      fprintf(fp, "\n%d", newNVert);
      for (int i=1; i<=newNVert; ++i) {
        fprintf(fp, " %5d", nodesk+newELMT(n, i));
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

  fprintf(fp, "\n\nCELL_TYPES %d", vtkTotalCells);

  for (int k=0; k<this->K; ++k) {
    fprintf(fp, "\n");
    for (int i=1; i<=Ncells; ++i) {
      fprintf(fp, "5 ");            // 5:VTK_TRIANGLE
      if (! (i%10))
        fprintf(fp, "\n");
    }
  }
  
  //-------------------------------------
  // 5. Write scalar "vtkCellData"
  //-------------------------------------

  // TODO: output "cell data" such as element area, 
  // or quality measures such as min/max angles, etc.

  // ...
  // ...


  // add final newline to output
  fprintf(fp, "\n");
  fclose(fp);
}


//---------------------------------------------------------
void NDG2D::Output_Mesh()
//---------------------------------------------------------
{
  static int count = 0;
  string output_dir = ".";

  // Write the triangles that underlie current DG mesh
  // in vtk format. Here we write the basic triangles 
  // either read from the original .neu file, or as 
  // adapted during adaptive mesh refinement (AMR).

  string buf = umOFORM("%s/mesh2D_N%d_%04d.vtk", output_dir.c_str(), this->N, ++count);
  FILE *fp = fopen(buf.c_str(), "w");
  if (!fp) {
    umLOG(1, "Could no open %s for output!\n", buf.c_str());
    return;
  }

  // Set flags and totals

  int Ncells = 1;   // 1 "cell" per triangle
  int Npts   = 3;   // 3 vertices per triangle

  this->Nv = this->VX.length();

  // set totals for Vtk output
  int vtkTotalPoints = this->Nv;
  int vtkTotalCells  = this->K * Ncells;
  int vtkTotalConns  = (this->EToV.num_cols()+1) * this->K * Ncells;


  //-------------------------------------
  // 1. Write the VTK header details
  //-------------------------------------
  fprintf(fp, "# vtk DataFile Version 2");
  fprintf(fp, "\nNuDG++ mesh");
  fprintf(fp, "\nASCII");
  fprintf(fp, "\nDATASET UNSTRUCTURED_GRID\n");
  fprintf(fp, "\nPOINTS %d double", vtkTotalPoints);

  //-------------------------------------
  // 2. Write the vertex data
  //-------------------------------------
  // write 2D vertex data to file
  for (int i=1; i<=this->Nv; ++i) {
    fprintf(fp, "\n%20.12e %20.12e  0.0", VX(i), VY(i));
  }

  //-------------------------------------
  // 3. Write the element connectivity
  //-------------------------------------

  // Number of indices required to define connectivity
  fprintf(fp, "\n\nCELLS %d %d", vtkTotalCells, vtkTotalConns);

  // write element connectivity to file
  for (int k=1; k<=this->K; ++k) {
    fprintf(fp, "\n3  %5d  %5d  %5d", 
                EToV(k,1)-1, EToV(k,2)-1, EToV(k,3)-1);
  }


  //-------------------------------------
  // 4. Write the cell types
  //-------------------------------------

  // For each element (cell) write a single integer 
  // identifying the cell type.  The integer should 
  // correspond to the enumeration in the vtk file:
  // /VTK/Filtering/vtkCellType.h

  fprintf(fp, "\n\nCELL_TYPES %d\n", vtkTotalCells);

  for (int k=1; k<=this->K; ++k) {
    fprintf(fp, "5 ");            // 5:VTK_TRIANGLE
    if (! (k%10))
      fprintf(fp, "\n");
  }
  
  //-------------------------------------
  // 5. Write scalar "vtkCellData"
  //-------------------------------------

  // TODO: output "cell data" such as element area, 
  // or quality measures such as min/max angles, etc.

  // TODO: "FaceData" i.e. boundary conditions
  // ...


  // add final newline to output
  fprintf(fp, "\n");
  fclose(fp);
}


//---------------------------------------------------------
void NDG2D::OutputSampleXYZ
(
        int sample_N,
        DMat &newX, 
        DMat &newY, 
        DMat &newZ,     // e.g. triangles on a sphere
  const DMat &FData,    // old field data
        DMat &newFData, // new field data
        int zfield      // if>0, use as z-elevation
)
//---------------------------------------------------------
{
  DVec newR, newS, newT;
  DMat newVDM;
  int newNpts = 0;

  // Triangles
  OutputSampleNodes2D(sample_N, newR, newS);
  newNpts = newR.size();
  newVDM = Vandermonde2D(this->N, newR, newS);

  const DMat& oldV = this->V;
  DMat oldtonew(newNpts, this->Np, "OldToNew");
  oldtonew = trans(trans(oldV) | trans(newVDM));

  //-----------------------------------
  // interpolate the field data
  //-----------------------------------
  int Nfields = FData.num_cols();
  newFData.resize(newNpts*this->K, Nfields);
  //DVec scales(Nfields);

  // For each field, use tOldF to wrap field i.
  // Use tNewF to load the interpolated field
  // directly into column i of the output array.
  DMat tOldF, tNewF;
  for (int i=1; i<=Nfields; ++i) {
    tOldF.borrow(this->Np, this->K, (double*)   FData.pCol(i));
    tNewF.borrow(newNpts,  this->K, (double*)newFData.pCol(i));
    tNewF = oldtonew * tOldF;
  //scales(i) = tNewF.max_col_val_abs(i);
  }

  //-----------------------------------
  // interpolate the vertices
  //-----------------------------------
  newX = oldtonew * this->x;
  newY = oldtonew * this->y;

  if (this->bCoord3D) {
    newZ = oldtonew * this->z;
  } 
  else 
  {
    if (zfield>=1 && zfield<=Nfields) {
      // use field data for z-height
      newZ.load(newNpts, K, newFData.pCol(Nfields));
    } else {
      // set z-data to 0.0
      newZ.resize(newNpts, K, true, 0.0);
    }
  }
}


//---------------------------------------------------------
void NDG2D::OutputSampleELMT2D(int sample_N, IMat& ELMT)
//---------------------------------------------------------
{
  int Nel = OutputSampleNelmt2D(sample_N);
  int Nvert=3, sk=0, i=0, j=0;
  int sampleNq = sample_N+1;
  IVec startrow(sampleNq);

  sk=0;
  for (i=0; i<sampleNq; ++i) {
    startrow[i] = sk;
    sk += sampleNq-i;
  }

  ELMT.resize(Nel,Nvert);

  // contruct triangulation
  sk = 1;
  for (i=0; i<sampleNq-1; ++i) {
    for (j=0; j<sampleNq-1-i; ++j, ++sk) {
      ELMT(sk,1) = startrow[i  ]+j;
      ELMT(sk,2) = startrow[i  ]+j+1;
      ELMT(sk,3) = startrow[i+1]+j;
    }
  }

  for (i=0; i<sampleNq-2; ++i) {
    for (j=0; j<sampleNq-2-i; ++j, ++sk) {
      ELMT(sk,1) = startrow[i  ]+j+1;
      ELMT(sk,2) = startrow[i+1]+j+1;
      ELMT(sk,3) = startrow[i+1]+j;
    }
  }
}


//---------------------------------------------------------
void NDG2D::Triangulation2D(int Np, IMat& elmtri)
//---------------------------------------------------------
{
  OutputSampleELMT2D(Np, elmtri);
  elmtri += 1;  // make indices 1-based 

#if (0)
  dumpIMat(elmtri, "elmtri");
  umLOG(1, "Check trangulation for Np = %d\n", Np);
//umERROR("Nigel", "Check trangulation for Np = %d", Np);
#endif
}


//---------------------------------------------------------
void NDG2D::OutputNodes(bool bFaceNodes)
//---------------------------------------------------------
{
  static int count = 0;
  string output_dir = ".";

  string buf = umOFORM("%s/mesh_N%02d_%04d.vtk", 
                      output_dir.c_str(), this->Nfp, ++count);

  FILE *fp = fopen(buf.c_str(), "w");
  if (!fp) {
    umLOG(1, "Could no open %s for output!\n", buf.c_str());
    return;
  }

  // Set flags and totals
 
  int Npoints = this->Np;  // volume nodes per element
  if (bFaceNodes)
    Npoints = Nfp*Nfaces;  // face nodes per element


  // set totals for Vtk output
#if (1)
  // FIXME: no connectivity
  int vtkTotalPoints = this->K * Npoints;
  int vtkTotalCells  = vtkTotalPoints;
  int vtkTotalConns  = vtkTotalPoints;
  int Ncells = Npoints;
#else
  int vtkTotalPoints = this->K * Npoints;
  int vtkTotalCells  = this->K * Ncells;
  int vtkTotalConns  = (this->EToV.num_cols()+1) * this->K * Ncells;
  int Ncells = this->N * this->N;
#endif

  //-------------------------------------
  // 1. Write the VTK header details
  //-------------------------------------
  fprintf(fp, "# vtk DataFile Version 2");
  fprintf(fp, "\nNDGFem simulation nodes");
  fprintf(fp, "\nASCII");
  fprintf(fp, "\nDATASET UNSTRUCTURED_GRID\n");
  fprintf(fp, "\nPOINTS %d double", vtkTotalPoints);

  int newNpts=0;

  //-------------------------------------
  // 2. Write the vertex data
  //-------------------------------------
  if (bFaceNodes) {
    for (int k=1; k<=this->K; ++k) {
      for (int n=1; n<=Npoints; ++n) {
        fprintf(fp, "\n%20.12e %20.12e %6.1lf", Fx(n,k), Fy(n,k), 0.0);
      }
    }
  } else {
    for (int k=1; k<=this->K; ++k) {
      for (int n=1; n<=Npoints; ++n) {
        fprintf(fp, "\n%20.12e %20.12e %6.1lf", x(n,k), y(n,k), 0.0);
      }
    }
  }

  //-------------------------------------
  // 3. Write the element connectivity
  //-------------------------------------

  // Number of indices required to define connectivity
  fprintf(fp, "\n\nCELLS %d %d", vtkTotalCells, 2*vtkTotalConns);


  // TODO: write element connectivity to file
  // FIXME: out-putting as VTK_VERTEX
  int nodesk=0;
  for (int k=0; k<this->K; ++k) {       // for each element
    for (int n=1; n<=Npoints; ++n) {
      fprintf(fp, "\n%d", 1);           // for each triangle
      for (int i=1; i<=1; ++i) {        // FIXME: no connectivity
        fprintf(fp, " %5d", nodesk);    // nodes in nth triangle
        nodesk++;                       // indexed from 0
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

  fprintf(fp, "\n\nCELL_TYPES %d", vtkTotalCells);

  for (int k=0; k<this->K; ++k) {
    fprintf(fp, "\n");
    for (int i=1; i<=Ncells; ++i) {
    //fprintf(fp, "5 ");            // 5:VTK_TRIANGLE
      fprintf(fp, "1 ");            // 1:VTK_VERTEX
      if (! (i%10))
        fprintf(fp, "\n");
    }
  }
  
  // add final newline to output
  fprintf(fp, "\n");
  fclose(fp);
}


//---------------------------------------------------------
void NDG2D::OutputNodes_cub()
//---------------------------------------------------------
{
  static int count = 0;
  string output_dir = ".";

  string buf = umOFORM("%s/cub_N%02d_%04d.vtk", 
                      output_dir.c_str(), m_cub.Ncub, ++count);

  FILE *fp = fopen(buf.c_str(), "w");
  if (!fp) {
    umLOG(1, "Could no open %s for output!\n", buf.c_str());
    return;
  }

  // Set flags and totals
 
  int Npoints = m_cub.Ncub;  // cubature nodes per element

  // set totals for Vtk output
#if (1)
  // FIXME: no connectivity
  int vtkTotalPoints = this->K * Npoints;
  int vtkTotalCells  = vtkTotalPoints;
  int vtkTotalConns  = vtkTotalPoints;
  int Ncells = Npoints;
#else
  int vtkTotalPoints = this->K * Npoints;
  int vtkTotalCells  = this->K * Ncells;
  int vtkTotalConns  = (this->EToV.num_cols()+1) * this->K * Ncells;
  int Ncells = this->N * this->N;
#endif

  //-------------------------------------
  // 1. Write the VTK header details
  //-------------------------------------
  fprintf(fp, "# vtk DataFile Version 2");
  fprintf(fp, "\nNDGFem simulation nodes (high-order cubature)");
  fprintf(fp, "\nASCII");
  fprintf(fp, "\nDATASET UNSTRUCTURED_GRID\n");
  fprintf(fp, "\nPOINTS %d double", vtkTotalPoints);

  int newNpts=0;

  //-------------------------------------
  // 2. Write the vertex data
  //-------------------------------------
  const DMat& cx=m_cub.x;
  const DMat& cy=m_cub.y;
  for (int k=1; k<=this->K; ++k) {
    for (int n=1; n<=Npoints; ++n) {
      fprintf(fp, "\n%20.12e %20.12e %6.1lf", cx(n,k), cy(n,k), 0.0);
    }
  }

  //-------------------------------------
  // 3. Write the element connectivity
  //-------------------------------------

  // Number of indices required to define connectivity
  fprintf(fp, "\n\nCELLS %d %d", vtkTotalCells, 2*vtkTotalConns);


  // TODO: write element connectivity to file
  // FIXME: out-putting as VTK_VERTEX
  int nodesk=0;
  for (int k=0; k<this->K; ++k) {       // for each element
    for (int n=1; n<=Npoints; ++n) {
      fprintf(fp, "\n%d", 1);           // for each triangle
      for (int i=1; i<=1; ++i) {        // FIXME: no connectivity
        fprintf(fp, " %5d", nodesk);    // nodes in nth triangle
        nodesk++;                       // indexed from 0
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

  fprintf(fp, "\n\nCELL_TYPES %d", vtkTotalCells);

  for (int k=0; k<this->K; ++k) {
    fprintf(fp, "\n");
    for (int i=1; i<=Ncells; ++i) {
    //fprintf(fp, "5 ");            // 5:VTK_TRIANGLE
      fprintf(fp, "1 ");            // 1:VTK_VERTEX
      if (! (i%10))
        fprintf(fp, "\n");
    }
  }
  
  // add final newline to output
  fprintf(fp, "\n");
  fclose(fp);
}


//---------------------------------------------------------
void NDG2D::OutputNodes_gauss()
//---------------------------------------------------------
{
  static int count = 0;
  string output_dir = ".";

  string buf = umOFORM("%s/gauss_N%02d_%04d.vtk", 
                      output_dir.c_str(), m_gauss.NGauss, ++count);

  FILE *fp = fopen(buf.c_str(), "w");
  if (!fp) {
    umLOG(1, "Could no open %s for output!\n", buf.c_str());
    return;
  }

  // Set flags and totals
 
  int Npoints = 3*m_gauss.NGauss;  // quadrature nodes per element

  // set totals for Vtk output
#if (1)
  // FIXME: no connectivity
  int vtkTotalPoints = this->K * Npoints;
  int vtkTotalCells  = vtkTotalPoints;
  int vtkTotalConns  = vtkTotalPoints;
  int Ncells = Npoints;
#else
  int vtkTotalPoints = this->K * Npoints;
  int vtkTotalCells  = this->K * Ncells;
  int vtkTotalConns  = (this->EToV.num_cols()+1) * this->K * Ncells;
  int Ncells = this->N * this->N;
#endif

  //-------------------------------------
  // 1. Write the VTK header details
  //-------------------------------------
  fprintf(fp, "# vtk DataFile Version 2");
  fprintf(fp, "\nNDGFem simulation nodes (surface quadrature)");
  fprintf(fp, "\nASCII");
  fprintf(fp, "\nDATASET UNSTRUCTURED_GRID\n");
  fprintf(fp, "\nPOINTS %d double", vtkTotalPoints);

  int newNpts=0;

  //-------------------------------------
  // 2. Write the vertex data
  //-------------------------------------
  const DMat& gx=m_gauss.x;
  const DMat& gy=m_gauss.y;
  for (int k=1; k<=this->K; ++k) {
    for (int n=1; n<=Npoints; ++n) {
      fprintf(fp, "\n%20.12e %20.12e %6.1lf", gx(n,k), gy(n,k), 0.0);
    }
  }

  //-------------------------------------
  // 3. Write the element connectivity
  //-------------------------------------

  // Number of indices required to define connectivity
  fprintf(fp, "\n\nCELLS %d %d", vtkTotalCells, 2*vtkTotalConns);


  // TODO: write element connectivity to file
  // FIXME: out-putting as VTK_VERTEX
  int nodesk=0;
  for (int k=0; k<this->K; ++k) {       // for each element
    for (int n=1; n<=Npoints; ++n) {
      fprintf(fp, "\n%d", 1);           // for each triangle
      for (int i=1; i<=1; ++i) {        // FIXME: no connectivity
        fprintf(fp, " %5d", nodesk);    // nodes in nth triangle
        nodesk++;                       // indexed from 0
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

  fprintf(fp, "\n\nCELL_TYPES %d", vtkTotalCells);

  for (int k=0; k<this->K; ++k) {
    fprintf(fp, "\n");
    for (int i=1; i<=Ncells; ++i) {
    //fprintf(fp, "5 ");            // 5:VTK_TRIANGLE
      fprintf(fp, "1 ");            // 1:VTK_VERTEX
      if (! (i%10))
        fprintf(fp, "\n");
    }
  }
  
  // add final newline to output
  fprintf(fp, "\n");
  fclose(fp);
}

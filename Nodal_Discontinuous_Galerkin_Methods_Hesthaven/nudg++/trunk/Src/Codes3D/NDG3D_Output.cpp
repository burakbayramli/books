// NDG3D_Output.cpp
// write selected fields to file
// 2007/10/16
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG3D.h"


//---------------------------------------------------------
// Utility routines
//---------------------------------------------------------

// find number of tetrahedra per element
int OutputSampleNelmt3D(int sample_N)
{
  return (sample_N-1)*(sample_N-1)*(sample_N-1);
}

// find number of nodes per face
int OutputSampleNpts3D(int sample_N)
{
  return sample_N*(sample_N+1)*(sample_N+2)/6;
}

// load {R,S,T} output nodes
void OutputSampleNodes3D(int sample_N, DVec& R, DVec& S, DVec& T)
{
  int Npts  = OutputSampleNpts3D(sample_N);
  R.resize(Npts); S.resize(Npts); T.resize(Npts);
  double denom = (double)(sample_N-1);
  for (int sk=1, i=0; i<sample_N; ++i) {
    for (int j=0; j<sample_N-i; ++j) {
      for (int k=0; k<sample_N-i-j; ++k, ++sk) {
        R(sk) = -1. + (2.*k)/denom;
        S(sk) = -1. + (2.*j)/denom;
        T(sk) = -1. + (2.*i)/denom;
      }
    }
  }
}


//---------------------------------------------------------
void NDG3D::OutputVTK(const DMat& FData, int order, int zfield)
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

  string buf = umOFORM("%s/s3D_N%02d_%04d.vtk", output_dir.c_str(), order, ++count);
  FILE *fp = fopen(buf.c_str(), "w");
  if (!fp) {
    umLOG(1, "Could no open %s for output!\n", buf.c_str());
    return;
  }

  // Set flags and totals
  int Output_N = std::max(2, order);
  int Ncells=0, Npts=0;

  Ncells = OutputSampleNelmt3D(Output_N);
  Npts   = OutputSampleNpts3D (Output_N);

  // set totals for Vtk output
  int vtkTotalPoints = this->K * Npts;
  int vtkTotalCells  = this->K * Ncells;
  int vtkTotalConns  = (this->EToV.num_cols()+1) * this->K * Ncells;


  //-------------------------------------
  // 1. Write the VTK header details
  //-------------------------------------
  fprintf(fp, "# vtk DataFile Version 2");
  fprintf(fp, "\nNuDG++ 3D simulation");
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

  // write 3D vertex data
  for (int k=1; k<=this->K; ++k) {
    for (int n=1; n<=newNpts; ++n) {
      // use exponential format to allow for
      // arbitrary (astro, nano) magnitudes:
      fprintf(fp, "\n%20.12e %20.12e %20.12e", 
                  newX(n, k), newY(n, k), newZ(n,k));
    }
  }


  //-------------------------------------
  // 3. Write the element connectivity
  //-------------------------------------
  IMat newELMT;

  // Number of indices required to define connectivity
  fprintf(fp, "\n\nCELLS %d %d", vtkTotalCells, vtkTotalConns);

  // build regularized tri elements at selected order
  OutputSampleELMT3D(Output_N, newELMT);
  newNpts = OutputSampleNpts3D(Output_N);

  int newNTet = newELMT.num_rows();
  int newNVert = newELMT.num_cols();

  // write element connectivity to file
  for (int k=0; k<this->K; ++k) {
    int nodesk = k*newNpts;
    for (int n=1; n<=newNTet; ++n) {
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
      fprintf(fp, "10 ");           // 10:VTK_TETRA
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
void NDG3D::Output_Mesh()
//---------------------------------------------------------
{
  static int count = 0;
  string output_dir = ".";

  // Write the tetrahedra that underlie current DG mesh
  // in vtk format. Here we write the basic tetrahedra 
  // either read from the original .neu file, or as 
  // adapted during adaptive mesh refinement (AMR).

  string buf = umOFORM("%s/mesh3D_N%d_%04d.vtk", output_dir.c_str(), this->N, ++count);
  FILE *fp = fopen(buf.c_str(), "w");
  if (!fp) {
    umLOG(1, "Could no open %s for output!\n", buf.c_str());
    return;
  }

  // Set flags and totals

  int Ncells = 1;   // 1 "cell" per tetrahedron
  int Npts   = 4;   // 4 vertices per tetrahedron

  this->Nv = this->VX.length();

  // set totals for Vtk output
  int vtkTotalPoints = this->Nv;
  int vtkTotalCells  = this->K * Ncells;
  int vtkTotalConns  = (this->EToV.num_cols()+1) * this->K * Ncells;


  //-------------------------------------
  // 1. Write the VTK header details
  //-------------------------------------
  fprintf(fp, "# vtk DataFile Version 2");
  fprintf(fp, "\nNuDG++ 3D mesh");
  fprintf(fp, "\nASCII");
  fprintf(fp, "\nDATASET UNSTRUCTURED_GRID\n");
  fprintf(fp, "\nPOINTS %d double", vtkTotalPoints);

  //-------------------------------------
  // 2. Write the vertex data
  //-------------------------------------
  // write 3D vertex data to file
  for (int i=1; i<=this->Nv; ++i) {
    fprintf(fp, "\n%20.12e %20.12e %20.12e", VX(i), VY(i), VZ(i));
  }

  //-------------------------------------
  // 3. Write the element connectivity
  //-------------------------------------

  // Number of indices required to define connectivity
  fprintf(fp, "\n\nCELLS %d %d", vtkTotalCells, vtkTotalConns);

  // write element connectivity to file
  for (int k=1; k<=this->K; ++k) {
    fprintf(fp, "\n4  %5d  %5d  %5d  %5d", 
                EToV(k,1)-1, EToV(k,2)-1, 
                EToV(k,3)-1, EToV(k,4)-1);
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
    fprintf(fp, "10 ");           // 10:VTK_TETRA
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
void NDG3D::OutputSampleXYZ
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

  // Tetrahedra
  OutputSampleNodes3D(sample_N, newR, newS, newT);
  newNpts = newR.size();
  newVDM = Vandermonde3D(this->N, newR,newS,newT);

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
  newZ = oldtonew * this->z;
}


//---------------------------------------------------------
void NDG3D::OutputSampleELMT3D(int sample_N, IMat& ELMT)
//---------------------------------------------------------
{
  int Nel   = OutputSampleNelmt3D(sample_N);
  int Nvert = 4;

  IMat startrow(sample_N, sample_N);

  for(int sk=0, i=0; i<sample_N; ++i) {
    for(int j=0; j<sample_N-i; ++j) {
      startrow(j+1, i+1) = sk;
      sk += sample_N-i-j;
    }
  }

  // contruct tetrahedralization
  ELMT.resize(Nel,Nvert);

  for(int i=0, sk=1; i<sample_N; ++i){
    for(int j=0; j<sample_N-i; ++j){
      for(int k=0; k<sample_N-i-j-1; ++k){
        // Add Tet 1
        ELMT(sk,1) = startrow(j+1, i+1)+k;
        ELMT(sk,2) = startrow(j+1, i+1)+k+1;
        ELMT(sk,3) = startrow(j+2, i+1)+k;
        ELMT(sk,4) = startrow(j+1, i+2)+k;
        ++sk;

        if(k < sample_N-i-j-2){
          // Add Tet 2
          ELMT(sk,1) = startrow(j+1, i+1)+k+1;
          ELMT(sk,2) = startrow(j+2, i+1)+k;
          ELMT(sk,3) = startrow(j+1, i+2)+k;
          ELMT(sk,4) = startrow(j+1, i+2)+k+1;
          ++sk;

          // Add Tet 3
          ELMT(sk,1) = startrow(j+1, i+1)+k+1;
          ELMT(sk,2) = startrow(j+2, i+1)+k+1;
          ELMT(sk,3) = startrow(j+2, i+1)+k;
          ELMT(sk,4) = startrow(j+1, i+2)+k+1;
          ++sk;

          // Add Tet 4
          ELMT(sk,1) = startrow(j+1, i+2)+k;
          ELMT(sk,2) = startrow(j+2, i+2)+k;
          ELMT(sk,3) = startrow(j+1, i+2)+k+1;
          ELMT(sk,4) = startrow(j+2, i+1)+k;
          ++sk;

          // Add Tet 5
          ELMT(sk,1) = startrow(j+2, i+1)+k;
          ELMT(sk,2) = startrow(j+2, i+1)+k+1;
          ELMT(sk,3) = startrow(j+2, i+2)+k;
          ELMT(sk,4) = startrow(j+1, i+2)+k+1;
          ++sk;
        }

        if(k < sample_N-i-j-3){
          // Add Tet 6
          ELMT(sk,1) = startrow(j+1, i+2)+k+1;
          ELMT(sk,2) = startrow(j+2, i+1)+k+1;
          ELMT(sk,3) = startrow(j+2, i+2)+k;
          ELMT(sk,4) = startrow(j+2, i+2)+k+1;
          ++sk;
        }
      }
    }
  }
}


//---------------------------------------------------------
void NDG3D::OutputNodes(bool bFaceNodes)
//---------------------------------------------------------
{
  static int count = 0;
  string output_dir = ".";

  string buf = umOFORM("%s/mesh_N%03d_%04d.vtk", 
                      output_dir.c_str(), this->Nfp, ++count);

  FILE *fp = fopen(buf.c_str(), "w");
  if (!fp) {
    umLOG(1, "Could no open %s for output!\n", buf.c_str());
    return;
  }

  // Set flags and totals
 
  int Npoints = this->Np;  // volume nodes per element
  if (bFaceNodes) {
    Npoints = Nfp*Nfaces;  // face nodes per element
  }

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
        fprintf(fp, "\n%20.12e %20.12e %6.1lf", Fx(n,k), Fy(n,k), Fz(n,k));
      }
    }
  } else {
    for (int k=1; k<=this->K; ++k) {
      for (int n=1; n<=Npoints; ++n) {
        fprintf(fp, "\n%20.12e %20.12e %6.1lf", x(n,k), y(n,k), z(n,k));
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
      fprintf(fp, "\n%d", 1);           // for each tetrahedron
      for (int i=1; i<=1; ++i) {        // FIXME: no connectivity
        fprintf(fp, " %5d", nodesk);    // nodes in nth tetrahedron
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
    //fprintf(fp, "10 ");           // 10: VTK_TETRA 
    //fprintf(fp, "5 ");            //  5:VTK_TRIANGLE
      fprintf(fp, "1 ");            //  1:VTK_VERTEX
      if (! (i%10))
        fprintf(fp, "\n");
    }
  }
  
  // add final newline to output
  fprintf(fp, "\n");
  fclose(fp);
}


//---------------------------------------------------------
void NDG3D::PlotContour3D
(
  int nn, 
  const DVec& field, 
  const DVec& cntrs
)
//---------------------------------------------------------
{
  umWARNING("NDG3D::PlotContour3D", "TODO: not implemented");
}

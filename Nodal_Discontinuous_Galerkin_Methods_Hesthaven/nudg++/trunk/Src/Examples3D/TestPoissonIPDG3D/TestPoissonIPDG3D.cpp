// TestPoissonIPDG3D.cpp
// Driver script for solving the 3D Poisson equation
// 2007/10/16
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "TestPoissonIPDG3D.h"


//---------------------------------------------------------
void TestPoissonIPDG3D::Driver()
//---------------------------------------------------------
{
  umLOG(1, "TestPoissonIPDG3D::Driver()\n");

  // polynomial order to use in each element
//N = 3;
  N = 4;
//N = 5;
  N = 6;

  //-------------------------------------------------------
  // Generate Mesh
  // figure(1); h = .33; 
  // [Nv, VX, VY, VZ, K, EToV] = MeshGenDistMesh3D(h);
  //-------------------------------------------------------
//FileName = "Grid/3D/Sphere_0448.neu";
//FileName = "Grid/3D/Sphere_1074.neu";
  FileName = "Grid/3D/F072.neu";
//FileName = "Grid/3D/cubeK6.neu";
//FileName = "Grid/3D/cubeK268.neu";
//FileName = "Grid/3D/cubeK1585.neu";
  //-------------------------------------------------------

  // Read in Mesh: [vertices, elements, materials, BC's]
  if (!MeshReaderGambit3D(FileName)) {
    umWARNING("TestPoissonIPDG3D::Driver", "Error loading mesh (file: %s)\nExiting.\n", FileName.c_str());
    return;
  }

  try {
    Run();
  } catch (...) {
    umWARNING("TestPoissonIPDG3D::Driver", "Caught exception from Run()");
    return;
  }
}


//---------------------------------------------------------
void TestPoissonIPDG3D::Run()
//---------------------------------------------------------
{
  umLOG(1, "TestPoissonIPDG3D::Run()\n");
  double wt1=timer.read();

  // Initialize solver and construct grid and metric
  StartUp3D();

#if (0)
  //-------------------------------------
  // check the mesh
  //-------------------------------------
  Output_Mesh();
  umLOG(1, "\n*** Exiting after writing mesh\n\n");
  return;
#endif

  // sparse operators
  CSd A("OP"), M("MM");

  // build 3D IPDG Poisson matrix (assuming all Dirichlet)
  PoissonIPDG3D(A, M);

  if (0) {
    // NBN: experiment with diagonal strength
    int Nz=0;
    for (int j=0; j<A.n; ++j) {
      for (int p = A.P[j]; p<A.P[j+1]; ++p) {
        if (A.I[p] == j) {
          A.X[p] += A.X[p]; // augment A(i,j)
        }
      }
    }
  }

  if (0) {
    umLOG(1, "Dumping file Afull.dat for Matlab\n");
    umLOG(1, "A(%d,%d), nnz = %d\n", A.m,A.n, A.P[A.n]);
    FILE* fp=fopen("Afull.dat", "w");
    A.write_ML(fp);
    fclose(fp);
    umLOG(1, "exiting.\n");
    return;
  }

  // iterative solver
  CS_PCG it_sol;

  try 
  {
    // Note: operator A is symmetric, with only its 
    // lower triangule stored.  We use this symmetry 
    // to accelerate operator A*x

    int flag=sp_SYMMETRIC; flag|=sp_LOWER; flag|=sp_TRIANGULAR;

    A.set_shape(flag);

    // drop tolerance for cholinc
    double droptol=1e-4;

    // Note: ownership of A is transfered to solver object
    it_sol.cholinc(A, droptol);

  } catch(...) {
    umLOG(1, "\nCaught exception from symbolic chol.\n");
  }


  DVec exact("exact"), f("f"), rhs("rhs"), u("u");
  double t1=0.0,t2=0.0;

  //-------------------------------------------------------
  // set up boundary condition 
  //-------------------------------------------------------
  DVec xbc = Fx(mapB), ybc = Fy(mapB), zbc = Fz(mapB);
  DVec ubc(Nfp*Nfaces*K);
  ubc(mapB) = sin(pi*xbc) * sin(pi*ybc) * sin(pi*zbc);

  //-------------------------------------------------------
  // form right hand side contribution from boundary condition
  //-------------------------------------------------------
  DMat Abc = PoissonIPDGbc3D(ubc);

  //-------------------------------------------------------
  // evaluate forcing function
  //-------------------------------------------------------
  exact = sin(pi*x).dm(sin(pi*y).dm(sin(pi*z)));
  f = (-3.0*SQ(pi)) * exact;

  //-------------------------------------------------------
  // set up right hand side for variational Poisson equation
  //-------------------------------------------------------
  rhs = M*(-f) + (DVec&)(Abc);

  //-------------------------------------------------------
  // solve using pcg iterative solver
  //-------------------------------------------------------
  t1 = timer.read();
  u  = it_sol.solve(rhs, 1e-9, 30);
  t2 = timer.read();

  //-------------------------------------------------------
  // compute nodal error
  //-------------------------------------------------------
  r = (u-exact);
  m_maxAbsError = r.max_val_abs();

  umLOG(1, "  solve -- done. (%0.4lf sec)\n\n", t2-t1);
  umLOG(1, "  max error = %g\n\n", m_maxAbsError);

#if (0)
  //#######################################################
  // plot solution and error
  figure(2);
  subplot(1,3,2); PlotContour3D(2*N, u, linspace(-1, 1, 10)); title('numerical solution');
  subplot(1,3,3); PlotContour3D(2*N, log10(eps+abs(u-exact)), linspace(-4, 0, 10)); 
  title('numerical error');
  //#######################################################
#endif

  double wt2=timer.read();
  umLOG(1, "TestPoissonIPDG3D::Run() complete\n");
  umLOG(1, "total time = %0.4lf sec\n\n", wt2-wt1);
}

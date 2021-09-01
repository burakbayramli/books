//-------------------------------- coords.h ---------------------------------
// Contains routines for transforming the coordinates of systems of particles
// Author: Titus Beu, 2013
//---------------------------------------------------------------------------
#ifndef _COORDS_
#define _COORDS_

#include "eigsys.h"

//===========================================================================
void MovetoCM(double m[], double x[], double y[], double z[], int n)
//---------------------------------------------------------------------------
// Moves a system of n particles to the CM system
//---------------------------------------------------------------------------
{
   double mCM, xCM, yCM, zCM;
   int i;

   mCM = 0e0; xCM = 0e0; yCM = 0e0; zCM = 0e0;
   for (i=1; i<=n; i++) {
      mCM += m[i];
      xCM += m[i] * x[i];
      yCM += m[i] * y[i];
      zCM += m[i] * z[i];
   }
   xCM /= mCM; yCM /= mCM; zCM /= mCM;

   for (i=1; i<=n; i++) {
      x[i] -= xCM; y[i] -= yCM; z[i] -= zCM;
   }
}

//===========================================================================
void PrincipalAxes(double m[], double x[], double y[], double z[], int n,
                   double MomInert[], int isort)
//---------------------------------------------------------------------------
// Rotates a set of n particles to the system of principal axes
// isort =  1 - highest symmetry axis along x-axis
// isort = -1 - highest symmetry axis along z-axis
//---------------------------------------------------------------------------
{
   double **Inert, **Rot;
   double mi, xi, yi, zi;
   int i, j;

   Inert = Matrix(1,3,1,3);
   Rot   = Matrix(1,3,1,3);

   for (i=1; i<=3; i++)
      for (j=1; j<=3; j++) Inert[i][j] = 0e0;

   for (i=1; i<=n; i++) {
      mi = m[i]; xi = x[i]; yi = y[i]; zi = z[i];
      Inert[1][1] += mi * (yi*yi + zi*zi);                  // inertia tensor
      Inert[2][2] += mi * (zi*zi + xi*xi);
      Inert[3][3] += mi * (xi*xi + yi*yi);
      Inert[2][1] -= mi * xi*yi;
      Inert[3][1] -= mi * zi*xi;
      Inert[3][2] -= mi * yi*zi;
   }

   Jacobi(Inert,Rot,MomInert,3);                // diagonalize inertia tensor
   EigSort(Rot,MomInert,3,isort);        // sort eigenvalues and eigenvectors

   for (i=1; i<=n; i++) {                  // rotate system to principal axes
      xi = x[i]; yi = y[i]; zi = z[i];
      x[i] = Rot[1][1] * xi + Rot[2][1] * yi + Rot[3][1] * zi;
      y[i] = Rot[1][2] * xi + Rot[2][2] * yi + Rot[3][2] * zi;
      z[i] = Rot[1][3] * xi + Rot[2][3] * yi + Rot[3][3] * zi;
   }
}

#endif

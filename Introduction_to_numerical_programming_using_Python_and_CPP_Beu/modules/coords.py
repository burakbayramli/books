#-------------------------------- coords.py ---------------------------------
#  Contains routines for transforming the coordinates of systems of particles
#  Author: Titus Beu, 2013
#----------------------------------------------------------------------------
from eigsys import *

#============================================================================
def MovetoCM(m, x, y, z, n):
#----------------------------------------------------------------------------
#  Moves a system of n particles to the CM system
#----------------------------------------------------------------------------
   mCM = 0e0; xCM = 0e0; yCM = 0e0; zCM = 0e0
   for i in range(1,n+1):
      mCM += m[i]
      xCM += m[i] * x[i]
      yCM += m[i] * y[i]
      zCM += m[i] * z[i]
   xCM /= mCM; yCM /= mCM; zCM /= mCM

   for i in range(1,n+1):
      x[i] -= xCM; y[i] -= yCM; z[i] -= zCM

#============================================================================
def PrincipalAxes(m, x, y, z, n, isort):
#----------------------------------------------------------------------------
#  Rotates a set of n particles to the system of principal axes
#  isort =  1 - highest symmetry axis along x-axis
#  isort = -1 - highest symmetry axis along z-axis
#----------------------------------------------------------------------------
   Inert = [[0]*4 for i in range(4)]
   Rot   = [[0]*4 for i in range(4)]
   MomInert = [0]*4

   for i in range(1,4):
      for j in range(1,i+1): Inert[i][j] = 0e0

   for i in range(1,n+1):
      mi = m[i]; xi = x[i]; yi = y[i]; zi = z[i]
      Inert[1][1] += mi * (yi*yi + zi*zi)                    # inertia tensor
      Inert[2][2] += mi * (zi*zi + xi*xi)
      Inert[3][3] += mi * (xi*xi + yi*yi)
      Inert[2][1] -= mi * xi*yi
      Inert[3][1] -= mi * zi*xi
      Inert[3][2] -= mi * yi*zi

   Jacobi(Inert,Rot,MomInert,3)                  # diagonalize inertia tensor
   EigSort(Rot,MomInert,3,isort)          # sort eigenvalues and eigenvectors

   for i in range(1,n+1):                   # rotate system to principal axes
      xi = x[i]; yi = y[i]; zi = z[i]
      x[i] = Rot[1][1] * xi + Rot[2][1] * yi + Rot[3][1] * zi
      y[i] = Rot[1][2] * xi + Rot[2][2] * yi + Rot[3][2] * zi
      z[i] = Rot[1][3] * xi + Rot[2][3] * yi + Rot[3][3] * zi

   return MomInert

//---------------------------------- pde.h ----------------------------------
// Contains routines for solving partial differential equations.
// Part of the numxlib numerics library. Author: Titus Beu, 2013
//---------------------------------------------------------------------------
#ifndef _PDE_
#define _PDE_

#include <math.h>
#include "linsys.h"

//===========================================================================
int Poisson0(double **u, double x[], double y[], int nx, int ny,
             double eps, double Func(double,double))
//---------------------------------------------------------------------------
// Solves the 2D Poisson equation in Cartesian coordinates with Dirichlet
// boundary conditions on a regular grid with (nx x ny) nodes (x[],y[]) using
// the Gauss-Seidel method. The solution u[][] is converged with relative
// precision eps. An error index is returned: 0 - normal execution.
// Calls: Func(x,y) - RHS of Poisson equation
//---------------------------------------------------------------------------
{
   const int itmax = 10000;                          // max no. of iterations
   double **f;
   double eij, err, hx, hy, kx, ky, kxy, uij;
   int i, it, j;

   f = Matrix(1,nx,1,ny);

   hx = (x[nx]-x[1])/(nx-1); kx = 1e0/(hx*hx);               // mesh spacings
   hy = (y[ny]-y[1])/(ny-1); ky = 1e0/(hy*hy);
   kxy = 2e0*(kx + ky);

   for (j=2; j<=ny-1; j++)                  // RHS of PDE for interior points
      for (i=2; i<=nx-1; i++) f[i][j] = Func(x[i],y[j]);

   for (it=1; it<=itmax; it++) {               // Gauss-Seidel iteration loop
      err = 0e0;
      for (j=2; j<=ny-1; j++) {
         for (i=2; i<=nx-1; i++) {                    // interior mesh points
            uij = (kx*(u[i-1][j] + u[i+1][j]) +
                   ky*(u[i][j-1] + u[i][j+1]) - f[i][j]) / kxy;
            eij = uij ? 1e0 - u[i][j]/uij : uij - u[i][j];     // local error
            if (fabs(eij) > err) err = fabs(eij);            // maximum error
            u[i][j] = uij;
         }
      }
      if (err <= eps) break;                              // convergence test
   }

   FreeMatrix(f,1,1);

   if (it > itmax) {
      printf("Poisson0: max. number of iterations exceeded !\n"); return 1;
   }
   return 0;
}

//===========================================================================
int PoissonXY(double **u, double x[], double y[], int nx, int ny,
              double eps, double Func(double,double),
   void CondX(double, double&, double&, double&, double&, double&, double&),
   void CondY(double, double&, double&, double&, double&, double&, double&))
//---------------------------------------------------------------------------
// Solves the 2D Poisson equation in Cartesian coordinates on a regular grid
// with (nx x ny) nodes (x[],y[]) using the Gauss-Seidel method. The solution
// u[][] is converged with relative precision eps.
// An error index is returned: 0 - normal execution.
// Calls: Func(x,y) - RHS of Poisson equation; boundary conditions:
//        CondX(y,alf_min,bet_min,gam_min,alf_max,bet_max,gam_max)
//        CondY(x,alf_min,bet_min,gam_min,alf_max,bet_max,gam_max)
//---------------------------------------------------------------------------
{
#define Save eij = uij ? 1e0 - u[i][j]/uij : uij - u[i][j]; \
             if (fabs(eij) > err) err = fabs(eij); \
             u[i][j] = uij;            // error estimate and solution storage

   const int itmax = 10000;                          // max no. of iterations
   double **f;
   double *betXmin, *betXmax, *gamXmin, *gamXmax;
   double *betYmin, *betYmax, *gamYmin, *gamYmax;
   double alf_min, bet_min, gam_min, alf_max, bet_max, gam_max;
   double eij, err, hx, hy, kx, ky, kxy, uij;
   int i, it, j;

   f = Matrix(1,nx,1,ny);
   betXmin = Vector(1,ny); betXmax = Vector(1,ny);
   gamXmin = Vector(1,ny); gamXmax = Vector(1,ny);
   betYmin = Vector(1,nx); betYmax = Vector(1,nx);
   gamYmin = Vector(1,nx); gamYmax = Vector(1,nx);

   hx = (x[nx]-x[1])/(nx-1); kx = 1e0/(hx*hx);               // mesh spacings
   hy = (y[ny]-y[1])/(ny-1); ky = 1e0/(hy*hy);
   kxy = 2e0*(kx + ky);

   for (j=2; j<=ny-1; j++)                  // RHS of PDE for interior points
      for (i=2; i<=nx-1; i++) f[i][j] = Func(x[i],y[j]);
                                                       // boundary conditions
   for (i=1; i<=nx; i++) {                      // lower and upper boundaries
      CondY(x[i],alf_min,bet_min,gam_min,alf_max,bet_max,gam_max);
      betYmin[i] = bet_min/(alf_min*hy + bet_min);
      gamYmin[i] = gam_min/(alf_min + bet_min/hy);
      betYmax[i] = bet_max/(alf_max*hy + bet_max);
      gamYmax[i] = gam_max/(alf_max + bet_max/hy);
   }
   for (j=2; j<=ny-1; j++) {                     // left and right boundaries
      CondX(y[j],alf_min,bet_min,gam_min,alf_max,bet_max,gam_max);
      betXmin[j] = bet_min/(alf_min*hx + bet_min);
      gamXmin[j] = gam_min/(alf_min + bet_min/hx);
      betXmax[j] = bet_max/(alf_max*hx + bet_max);
      gamXmax[j] = gam_max/(alf_max + bet_max/hx);
   }

   for (it=1; it<=itmax; it++) {               // Gauss-Seidel iteration loop
      err = 0e0;
      j = 1;                                                // lower boundary
      for (i=1; i<=nx; i++) {
         uij = betYmin[i]*u[i][2] + gamYmin[i]; Save
      }
      for (j=2; j<=ny-1; j++) {
         i = 1;                                              // left boundary
         uij = betXmin[j]*u[i+1][j] + gamXmin[j]; Save
         for (i=2; i<=nx-1; i++) {                    // interior mesh points
            uij = (kx*(u[i-1][j] + u[i+1][j]) +
                   ky*(u[i][j-1] + u[i][j+1]) - f[i][j]) / kxy; Save
         }
         i = nx;                                            // right boundary
         uij = betXmax[j]*u[i-1][j] + gamXmax[j]; Save
      }
      j = ny;                                               // upper boundary
      for (i=1; i<=nx; i++) {
         uij = betYmax[i]*u[i][ny-1] + gamYmax[i]; Save
      }
      if (err <= eps) break;                              // convergence test
   }

   FreeMatrix(f,1,1);
   FreeVector(betXmin,1); FreeVector(betXmax,1);
   FreeVector(gamXmin,1); FreeVector(gamXmax,1);
   FreeVector(betYmin,1); FreeVector(betYmax,1);
   FreeVector(gamYmin,1); FreeVector(gamYmax,1);

   if (it > itmax) {
      printf("PoissonXY: max. number of iterations exceeded !\n"); return 1;
   }
   return 0;
}

//===========================================================================
int Poisson2D(double **u, double x[], double y[],
              int imin[], int imax[], int nx, int ny, double eps,
              double Func(double,double))
//---------------------------------------------------------------------------
// Solves the 2D Poisson equation in Cartesian coordinates on a regular grid
// with (nx x ny) nodes (x[],y[]), with limiting indexes along x specified by
// imin[j] and imax[j] (j=1,ny), using the Gauss-Seidel method. The solution
// u[][] is converged with relative precision eps.
// An error index is returned: 0 - normal execution.
// Calls: Func(x,y) - RHS of Poisson equation
//---------------------------------------------------------------------------
{
   const int itmax = 10000;                          // max no. of iterations
   double **f;
   double eij, err, hx, hy, kx, ky, kxy, uij;
   int i, it, j;

   f = Matrix(1,nx,1,ny);

   hx = (x[nx]-x[1])/(nx-1); kx = 1e0/(hx*hx);               // mesh spacings
   hy = (y[ny]-y[1])/(ny-1); ky = 1e0/(hy*hy);
   kxy = 2e0*(kx + ky);

   for (j=2; j<=ny-1; j++)                  // RHS of PDE for interior points
      for (i=imin[j]+1; i<=imax[j]-1; i++) f[i][j] = Func(x[i],y[j]);

   for (it=1; it<=itmax; it++) {               // Gauss-Seidel iteration loop
      err = 0e0;
      for (j=2; j<=ny-1; j++) {
         for (i=imin[j]+1; i<=imax[j]-1; i++) {       // interior mesh points
            uij = (kx*(u[i-1][j] + u[i+1][j]) - f[i][j]) / kxy;
            if (i >= imin[j-1] && i <= imax[j-1]) uij += ky*u[i][j-1] / kxy;
            if (i >= imin[j+1] && i <= imax[j+1]) uij += ky*u[i][j+1] / kxy;

            eij = uij ? 1e0 - u[i][j]/uij : uij - u[i][j]; // error estimate
            if (fabs(eij) > err) err = fabs(eij);    // max. component error
            u[i][j] = uij;
         }
      }
      if (err <= eps) break;                              // convergence test
   }

   FreeMatrix(f,1,1);

   if (it > itmax) {
      printf("Poisson2D: max. number of iterations exceeded !\n"); return 1;
   }
   return 0;
}

//===========================================================================
void PropagFTCS(double u0[], double u[], int nx, double D, double hx,
                double ht)
//---------------------------------------------------------------------------
// Propagates the solution u0[] of the diffusion equation
//    du/dt = D d2u/dx2,  D - diffusion coefficient (constant)
// over the time intervat ht, using an explicit FTCS difference scheme on
// a spatial grid with nx nodes and spacing hx. Returns the solution in u[].
//---------------------------------------------------------------------------
{
   double lam, lam1;
   int i;

   lam = D * ht/(hx*hx);
   lam1 = 1e0 - 2e0*lam;

   u[1] = u0[1]; u[nx] = u0[nx];
   for (i=2; i<=(nx-1); i++)
      u[i] = lam*u0[i-1] + lam1*u0[i] + lam*u0[i+1];
}

//===========================================================================
void PropagBTCS(double u0[], double u[], int nx, double D, double hx,
                double ht)
//---------------------------------------------------------------------------
// Propagates the solution u0[] of the diffusion equation
//    du/dt = D d2u/dx2,  D - diffusion coefficient (constant)
// over the time intervat ht, using an implicit BTCS difference scheme on
// a spatial grid with nx nodes and spacing hx. Returns the solution in u[].
//---------------------------------------------------------------------------
{
   static double *a, *b, *c;
   double lam, lam1;
   static int init = 1;                              // initialization switch
   int i;

   if (init)
      { a = Vector(1,nx); b = Vector(1,nx); c = Vector(1,nx); init = 0; }

   lam = D * ht/(hx*hx);
   lam1 = 1e0 + 2e0*lam;

   b[1] = 1e0; c[1] = 0e0;        // build coefficients of discretized system
   u[1] = u0[1];
   for (i=2; i<=nx-1; i++) {
      a[i] = -lam; b[i] = lam1; c[i] = -lam;
      u[i] = u0[i];
   }
   a[nx] = 0e0; b[nx] = 1e0;
   u[nx] = u0[nx];

   TriDiagSys(a,b,c,u,nx);            // solve tridiagonal discretized system
}

//===========================================================================
void PropagCN(double u0[], double u[], int nx, double D, double hx, double ht)
//---------------------------------------------------------------------------
// Propagates the solution u0[] of the diffusion equation
//    du/dt = D d2u/dx2,  D - diffusion coefficient (constant)
// over the time intervat ht, using the Crank-Nicolson difference scheme on
// a spatial grid with nx nodes and spacing hx. Returns the solution in u[].
//---------------------------------------------------------------------------
{
   static double *a, *b, *c;
   double lam, lam1, lam2;
   static int init = 1;                              // initialization switch
   int i;

   if (init)
      { a = Vector(1,nx); b = Vector(1,nx); c = Vector(1,nx); init = 0; }

   lam = 0.5e0 * D * ht/(hx*hx);
   lam1 = 1e0 + 2e0*lam; lam2 = 1e0 - 2e0*lam;

   b[1] = 1e0; c[1] = 0e0;        // build coefficients of discretized system
   u[1] = u0[1];
   for (i=2; i<=(nx-1); i++) {
      a[i] = -lam; b[i] = lam1; c[i] = -lam;
      u[i] = lam*u0[i-1] + lam2*u0[i] + lam*u0[i+1];
   }
   a[nx] = 0e0; b[nx] = 1e0;
   u[nx] = u0[nx];

   TriDiagSys(a,b,c,u,nx);            // solve tridiagonal discretized system
}

//===========================================================================
void PropagDiff(double u0[], double u[], double D[], int nx, double hx,
             double ht, int iopBC1, int iopBC2, double Jdiff1, double Jdiff2)
//---------------------------------------------------------------------------
// Propagates the solution u0[] of the diffusion equation
//    du/dt = D d2u/dx2,  D - diffusion coefficient (spatially variable)
// over the time intervat ht, using the Crank-Nicolson difference scheme on
// a spatial grid with nx nodes and spacing hx. Returns the solution in u[].
// iopBC1, iopBC2 - left/right boundary condition: 0 - Dirichlet, 1 - Neumann
// Jdiff1, Jdiff2 - left/right boundary fluxes for Neumann conditions
//---------------------------------------------------------------------------
{
   static double *a, *b, *c;
   double f, lam, lam1, lam2, uint, uint0;
   static int init = 1;                              // initialization switch
   int i;

   if (init)
      { a = Vector(1,nx); b = Vector(1,nx); c = Vector(1,nx); init = 0; }

   f = 0.5e0 * ht/(hx*hx);

   b[1] = 1e0; c[1] = -iopBC1;    // build coefficients of discretized system
   u[1] = iopBC1 ? hx*Jdiff1/D[1] : u0[1];
   for (i=2; i<=(nx-1); i++) {
      lam = D[i] * f;
      lam1 = 0.5e0 * (D[i] + D[i-1]) * f;
      lam2 = 0.5e0 * (D[i] + D[i+1]) * f;
      a[i] = -lam1;
      b[i] = 1e0 + 2e0*lam;
      c[i] = -lam2;
      u[i] = lam1*u0[i-1] + (1e0 - 2e0*lam)*u0[i] + lam2*u0[i+1];
   }
   a[nx] = -iopBC2; b[nx] = 1e0;
   u[nx] = iopBC2 ? -hx*Jdiff2/D[nx] : u0[nx];

   TriDiagSys(a,b,c,u,nx);            // solve tridiagonal discretized system

   for (i=1; i<=nx; i++) if (u[i] < 0e0) u[i] = 0e0;    // keep solution >= 0

   uint0 = 0.5e0*(u0[1] + u0[nx]);                // integral of old solution
   for (i=2; i<=nx-1; i++) uint0 += u0[i];                // trapezoidal rule
   uint0 *= hx;
   if (iopBC1 == 1) uint0 += Jdiff1*ht;          // contribution of left flux
   if (iopBC2 == 1) uint0 -= Jdiff2*ht;         // contribution of right flux
   
   uint = 0.5e0*(u[1] + u[nx]);                   // integral of new solution
   for (i=2; i<=nx-1; i++) uint += u[i];                  // trapezoidal rule
   uint *= hx;

   f = uint0/uint;                                    // normalization factor
   if (f < 0e0) f = 0e0;
   for (i=1; i<=nx; i++) u[i] *= f;                     // normalize solution
}

//===========================================================================
void PropagQTD(dcmplx Psi[], double V[], int nx, double hx, double ht)
//---------------------------------------------------------------------------
// Propagates the solution PSI = Psi + i Chi of the 1D Schrodinger equation
//   i d/dt PSI(x,t) = [-(1/2) d2/dx2 - V(x)] PSI(x,t),
// over the time interval ht. Uses the Crank-Nicolson scheme on a grid with
// nx nodes and spacing hx and solves the tridiagonal discretized system by
// LU factorization. Uses complex arithmetic.
//---------------------------------------------------------------------------
{
   static dcmplx *a, *b, *c;               // diagonals of discretized system
   dcmplx Psii, Psi1;
   double lam, W;
   static int init = 1;
   int i;

   if (init)
      { a = CVector(1,nx); b = CVector(1,nx); c = CVector(1,nx); init = 0; }

   lam = ht/(4e0*hx*hx);

   b[1] = 1e0; c[1] = 0e0;        // build coefficients of discretized system
   Psii = Psi[1];
   for (i=2; i<=(nx-1); i++) {
      Psi1 = Psii; Psii = Psi[i];          // save initial wave packet values
      W = 2e0*lam + 0.5e0*ht*V[i];
      a[i] = -lam;
      b[i] = dcmplx(W,-1e0);
      c[i] = -lam;
      Psi[i] = lam*Psi1 - dcmplx(W,1e0)*Psii + lam*Psi[i+1]; // constant term
   }
   a[nx] = 0e0; b[nx] = 1e0;
                                      // solve tridiagonal discretized system
   TriDiagSys(a,b,c,Psi,nx);          // solution Psi: propagated wave packet
}

//===========================================================================
int PropagQGS(double Psi[], double Chi[], double V[], int nx, double hx,
              double ht)
//---------------------------------------------------------------------------
// Propagates the solution PSI = Psi + i Chi of the 1D Schrodinger equation
//   i d/dt PSI(x,t) = [-(1/2) d2/dx2 - V(x)] PSI(x,t),
// over the time interval ht. Uses the Crank-Nicolson scheme on a grid with
// nx nodes and spacing hx and solves the discretized system by Gauss-Seidel
// iterations. Uses real arithmetic.
//---------------------------------------------------------------------------
{
   const double eps = 1e-6;                             // relative tolerance
   const int itmax = 100;                            // max no. of iterations
   static double *Psi0, *Chi0, *W;
   double err, erri, lam, Psii, Chii;
   static int init = 1;
   int i, it;

   if (init) {
      Psi0 = Vector(1,nx); Chi0 = Vector(1,nx); W = Vector(1,nx); init = 0;
   }

   lam = ht/(4e0*hx*hx);

   for (i=2; i<=nx-1; i++) {             // explicit 0th-order approximations
      W[i] = 0.5e0*ht*V[i] + 2e0*lam;
      Psi0[i] = Psi[i] - lam*(Chi[i-1] + Chi[i+1]) + W[i]*Chi[i];
      Chi0[i] = Chi[i] + lam*(Psi[i-1] + Psi[i+1]) - W[i]*Psi[i];
   }

   for (it=1; it<=itmax; it++) {               // Gauss-Seidel iteration loop
      err = 0e0;
      for (i=2; i<=nx-1; i++) {
         Psii = Psi0[i] - lam*(Chi[i-1] + Chi[i+1]) + W[i]*Chi[i];
         Chii = Chi0[i] + lam*(Psi[i-1] + Psi[i+1]) - W[i]*Psii;
                         // local error estimate based on probability density
         erri = fabs((Psii*Psii+Chii*Chii) - (Psi[i]*Psi[i]+Chi[i]*Chi[i]));
         if (erri > err) err = erri;                // maximum error estimate
         Psi[i] = Psii;
         Chi[i] = Chii;
      }
      if (err <= eps) break;                              // convergence test
   }

   if (it > itmax) {
      printf("PropagQMGS: max. number of iterations exceeded !\n"); return 1;
   }
   return 0;
}

//===========================================================================
void PropagWave(double u0[], double u1[], double u[], int nx, double c,
                double hx, double ht)
//---------------------------------------------------------------------------
// Propagates the solutions u0[] and u1[] of the wave equation
//    d2u/dt2 = c^2 d2u/dx2,  c - phase velocity (constant)
// over the time interval ht, using the explicit difference scheme on a
// spatial grid with nx nodes and spacing hx. Returns the solution in u[].
//---------------------------------------------------------------------------
{
   double lam, lam2;
   int i;

   lam = c*ht/hx; lam = lam*lam;
   lam2 = 2e0*(1e0 - lam);

   u[1] = u0[1]; u[nx] = u0[nx];             // Dirichlet boundary conditions
   for (i=2; i<=nx-1; i++)           // propagate solution at interior points
      u[i] = lam*u1[i-1] + lam2*u1[i] + lam*u1[i+1] - u0[i];
}

#endif
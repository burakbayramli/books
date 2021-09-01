#---------------------------------- pde.py ----------------------------------
#  Contains routines for solving partial differential equations.
#  Part of the numxlib numerics library. Author: Titus Beu, 2013
#----------------------------------------------------------------------------
from math import *
from linsys import *

#============================================================================
def Poisson0(u, x, y, nx, ny, eps, Func):
#----------------------------------------------------------------------------
#  Solves the 2D Poisson equation in Cartesian coordinates with Dirichlet
#  boundary conditions on a regular grid with (nx x ny) nodes (x[],y[]) using
#  the Gauss-Seidel method. The solution u[][] is converged with relative
#  precision eps. An error index is returned: 0 - normal execution.
#  Calls: Func(x,y) - RHS of Poisson equation
#----------------------------------------------------------------------------
   itmax = 10000                                      # max no. of iterations

   f = [[0]*(ny+1) for i in range(nx+1)]

   hx = (x[nx]-x[1])/(nx-1); kx = 1e0/(hx*hx)                 # mesh spacings
   hy = (y[ny]-y[1])/(ny-1); ky = 1e0/(hy*hy) 
   kxy = 2e0*(kx + ky)

   for j in range(2,ny):                     # RHS of PDE for interior points
      for i in range(2,nx): f[i][j] = Func(x[i],y[j])

   for it in range(1,itmax+1):                  # Gauss-Seidel iteration loop
      err = 0e0
      for j in range(2,ny):
         for i in range(2,nx):                         # interior mesh points
            uij = (kx*(u[i-1][j] + u[i+1][j]) +
                   ky*(u[i][j-1] + u[i][j+1]) - f[i][j]) / kxy
            eij = 1e0 - u[i][j]/uij if uij else uij - u[i][j]   # local error
            if (fabs(eij) > err): err = fabs(eij)             # maximum error
            u[i][j] = uij

      if (err <= eps): break                               # convergence test

   if (it >= itmax):
      print("Poisson0: max. number of iterations exceeded !"); return 1
   return 0

#============================================================================
def PoissonXY(u, x, y, nx, ny, eps, Func, CondX, CondY):
#----------------------------------------------------------------------------
#  Solves the 2D Poisson equation in Cartesian coordinates on a regular grid
#  with (nx x ny) nodes (x[],y[]) using the Gauss-Seidel method. The solution
#  u[][] is converged with relative precision eps.
#  An error index is returned: 0 - normal execution.
#  Calls: Func(x,y) - RHS of Poisson equation; boundary conditions:
#         CondX(y,alf_min,bet_min,gam_min,alf_max,bet_max,gam_max)
#         CondY(x,alf_min,bet_min,gam_min,alf_max,bet_max,gam_max)
#----------------------------------------------------------------------------
   itmax = 10000                                      # max no. of iterations

   f = [[0]*(ny+1) for i in range(nx+1)]
   betXmin = [0]*(ny+1); betXmax = [0]*(ny+1)
   gamXmin = [0]*(ny+1); gamXmax = [0]*(ny+1)
   betYmin = [0]*(nx+1); betYmax = [0]*(nx+1)
   gamYmin = [0]*(nx+1); gamYmax = [0]*(nx+1)

   hx = (x[nx]-x[1])/(nx-1); kx = 1e0/(hx*hx)                 # mesh spacings
   hy = (y[ny]-y[1])/(ny-1); ky = 1e0/(hy*hy) 
   kxy = 2e0*(kx + ky)

   for j in range(2,ny):                     # RHS of PDE for interior points
      for i in range(2,nx): f[i][j] = Func(x[i],y[j])
                                                        # boundary conditions
   for i in range(1,nx+1):                       # lower and upper boundaries
      (alf_min,bet_min,gam_min,alf_max,bet_max,gam_max) = CondY(x[i])
      betYmin[i] = bet_min/(alf_min*hy + bet_min)
      gamYmin[i] = gam_min/(alf_min + bet_min/hy)
      betYmax[i] = bet_max/(alf_max*hy + bet_max)
      gamYmax[i] = gam_max/(alf_max + bet_max/hy)

   for j in range(2,ny):                          # left and right boundaries
      (alf_min,bet_min,gam_min,alf_max,bet_max,gam_max) = CondX(y[j])
      betXmin[j] = bet_min/(alf_min*hx + bet_min)
      gamXmin[j] = gam_min/(alf_min + bet_min/hx)
      betXmax[j] = bet_max/(alf_max*hx + bet_max)
      gamXmax[j] = gam_max/(alf_max + bet_max/hx)

   for it in range(1,itmax+1):                  # Gauss-Seidel iteration loop
      err = 0e0
      j = 1                                                  # lower boundary
      for i in range(1,nx+1):
         uij = betYmin[i]*u[i][2] + gamYmin[i]
         eij = 1e0 - u[i][j]/uij if uij else uij - u[i][j]
         if (fabs(eij) > err): err = fabs(eij)
         u[i][j] = uij

      for j in range(2,ny):
         i = 1                                                # left boundary
         uij = betXmin[j]*u[i+1][j] + gamXmin[j]
         eij = 1e0 - u[i][j]/uij if uij else uij - u[i][j]
         if (fabs(eij) > err): err = fabs(eij)
         u[i][j] = uij

         for i in range(2,nx):                         # interior mesh points
            uij = (kx*(u[i-1][j] + u[i+1][j]) +
                   ky*(u[i][j-1] + u[i][j+1]) - f[i][j]) / kxy
            eij = 1e0 - u[i][j]/uij if uij else uij - u[i][j]   # local error
            if (fabs(eij) > err): err = fabs(eij)             # maximum error
            u[i][j] = uij

         i = nx                                              # right boundary
         uij = betXmax[j]*u[i-1][j] + gamXmax[j]
         eij = 1e0 - u[i][j]/uij if uij else uij - u[i][j]
         if (fabs(eij) > err): err = fabs(eij)
         u[i][j] = uij

      j = ny                                                 # upper boundary
      for i in range(1,nx+1):
         uij = betYmax[i]*u[i][ny-1] + gamYmax[i]
         eij = 1e0 - u[i][j]/uij if uij else uij - u[i][j]
         if (fabs(eij) > err): err = fabs(eij)
         u[i][j] = uij

      if (err <= eps): break                               # convergence test

   if (it >= itmax):
      print("PoissonXY: max. number of iterations exceeded !"); return 1
   return 0

#============================================================================
def Poisson2D(u, x, y, imin, imax, nx, ny, eps, Func):
#----------------------------------------------------------------------------
#  Solves the 2D Poisson equation in Cartesian coordinates on a regular grid
#  with (nx x ny) nodes (x[],y[]), with limiting indexes along x specified by
#  imin[j] and imax[j] (j=1,ny), using the Gauss-Seidel method. The solution
#  u[][] is converged with relative precision eps.
#  An error index is returned: 0 - normal execution.
#  Calls: Func(x,y) - RHS of Poisson equation
#----------------------------------------------------------------------------
   itmax = 10000                                      # max no. of iterations

   f = [[0]*(ny+1) for i in range(nx+1)]

   hx = (x[nx]-x[1])/(nx-1); kx = 1e0/(hx*hx)                 # mesh spacings
   hy = (y[ny]-y[1])/(ny-1); ky = 1e0/(hy*hy) 
   kxy = 2e0*(kx + ky)

   for j in range(2,ny):                     # RHS of PDE for interior points
      for i in range(2,nx): f[i][j] = Func(x[i],y[j])

   for it in range(1,itmax+1):                  # Gauss-Seidel iteration loop
      err = 0e0
      for j in range(2,ny):
         for i in range(imin[j]+1,imax[j]):            # interior mesh points
            uij = (kx*(u[i-1][j] + u[i+1][j]) - f[i][j]) / kxy
            if (i >= imin[j-1] and i <= imax[j-1]): uij += ky*u[i][j-1] / kxy
            if (i >= imin[j+1] and i <= imax[j+1]): uij += ky*u[i][j+1] / kxy

            eij = 1e0 - u[i][j]/uij if uij else uij - u[i][j]
            if (fabs(eij) > err): err = fabs(eij)      # max. component error
            u[i][j] = uij

      if (err <= eps): break                               # convergence test

   if (it >= itmax):
      print("Poisson2D: max. number of iterations exceeded !"); return 1
   return 0

#============================================================================
def PropagFTCS(u0, u, nx, D, hx, ht):
#----------------------------------------------------------------------------
#  Propagates the solution u0[] of the diffusion equation
#     du/dt = D d2u/dx2,  D - diffusion coefficient (constant)
#  over the time intervat ht, using an explicit FTCS difference scheme on
#  a spatial grid with nx nodes and spacing hx. Returns the solution in u[].
#----------------------------------------------------------------------------
   lam = D * ht/(hx*hx) 
   lam1 = 1e0 - 2e0*lam

   u[1] = u0[1]; u[nx] = u0[nx]
   for i in range(2,nx):
      u[i] = lam*u0[i-1] + lam1*u0[i] + lam*u0[i+1]

#============================================================================
def PropagBTCS(u0, u, nx, D, hx, ht):
#----------------------------------------------------------------------------
#  Propagates the solution u0[] of the diffusion equation
#     du/dt = D d2u/dx2,  D - diffusion coefficient (constant)
#  over the time intervat ht, using an implicit BTCS difference scheme on
#  a spatial grid with nx nodes and spacing hx. Returns the solution in u[].
#----------------------------------------------------------------------------
   a = [0]*(nx+1); b = [0]*(nx+1); c = [0]*(nx+1)

   lam = D * ht/(hx*hx)
   lam1 = 1e0 + 2e0*lam

   b[1] = 1e0; c[1] = 0e0          # build coefficients of discretized system
   u[1] = u0[1]
   for i in range(2,nx):
      a[i] = -lam; b[i] = lam1; c[i] = -lam
      u[i] = u0[i]
   a[nx] = 0e0; b[nx] = 1e0
   u[nx] = u0[nx]

   TriDiagSys(a,b,c,u,nx)              # solve tridiagonal discretized system

#============================================================================
def PropagCN(u0, u, nx, D, hx, ht):
#----------------------------------------------------------------------------
#  Propagates the solution u0[] of the diffusion equation
#     du/dt = D d2u/dx2,  D - diffusion coefficient (constant)
#  over the time intervat ht, using the Crank-Nicolson difference scheme on
#  a spatial grid with nx nodes and spacing hx. Returns the solution in u[].
#----------------------------------------------------------------------------
   a = [0]*(nx+1); b = [0]*(nx+1); c = [0]*(nx+1)

   lam = 0.5e0 * D * ht/(hx*hx)
   lam1 = 1e0 + 2e0*lam; lam2 = 1e0 - 2e0*lam

   b[1] = 1e0; c[1] = 0e0          # build coefficients of discretized system
   u[1] = u0[1]
   for i in range(2,nx):
      a[i] = -lam; b[i] = lam1; c[i] = -lam
      u[i] = lam*u0[i-1] + lam2*u0[i] + lam*u0[i+1]

   a[nx] = 0e0; b[nx] = 1e0
   u[nx] = u0[nx]

   TriDiagSys(a,b,c,u,nx)              # solve tridiagonal discretized system

#============================================================================
def PropagDiff(u0, u, D, nx, hx, ht, iopBC1, iopBC2, Jdiff1, Jdiff2):
#----------------------------------------------------------------------------
#  Propagates the solution u0[] of the diffusion equation
#     du/dt = D d2u/dx2,  D - diffusion coefficient (spatially variable)
#  over the time intervat ht, using the Crank-Nicolson difference scheme on
#  a spatial grid with nx nodes and spacing hx. Returns the solution in u[].
#  iopBC1, iopBC2 - left/right boundary condition: 0 - Dirichlet, 1 - Neumann
#  Jdiff1, Jdiff2 - left/right boundary fluxes for Neumann conditions
# ---------------------------------------------------------------------------
   a = [0]*(nx+1); b = [0]*(nx+1); c = [0]*(nx+1)

   f = 0.5e0 * ht/(hx*hx)

   b[1] = 1e0; c[1] = -iopBC1      # build coefficients of discretized system
   u[1] = hx*Jdiff1/D[1] if iopBC1 else u0[1]
   for i in range(2,nx):
      lam = D[i] * f
      lam1 = 0.5e0 * (D[i] + D[i-1]) * f
      lam2 = 0.5e0 * (D[i] + D[i+1]) * f
      a[i] = -lam1
      b[i] = 1e0 + 2e0*lam
      c[i] = -lam2
      u[i] = lam1*u0[i-1] + (1e0 - 2e0*lam)*u0[i] + lam2*u0[i+1]

   a[nx] = -iopBC2; b[nx] = 1e0
   u[nx] = -hx*Jdiff2/D[nx] if iopBC2 else u0[nx]

   TriDiagSys(a,b,c,u,nx)              # solve tridiagonal discretized system

   for i in range(1,nx+1):                               # keep solution >= 0
      if (u[i] < 0e0): u[i] = 0e0

   uint0 = 0.5e0*(u0[1] + u0[nx])                  # integral of old solution
   for i in range(2,nx): uint0 += u0[i]                    # trapezoidal rule
   uint0 *= hx
   if (iopBC1 == 1): uint0 += Jdiff1*ht           # contribution of left flux
   if (iopBC2 == 1): uint0 -= Jdiff2*ht          # contribution of right flux
   
   uint = 0.5e0*(u[1] + u[nx])                     # integral of new solution
   for i in range(2,nx): uint += u[i]                      # trapezoidal rule
   uint *= hx

   f = uint0/uint                                      # normalization factor
   if (f < 0e0): f = 0e0
   for i in range(1,nx+1): u[i] *= f                     # normalize solution

#============================================================================
def PropagQTD(Psi, V, nx, hx, ht):
#----------------------------------------------------------------------------
#  Propagates the solution PSI = Psi + i Chi of the 1D Schrodinger equation
#    i d/dt PSI(x,t) = [-(1/2) d2/dx2 - V(x)] PSI(x,t),
#  over the time interval ht. Uses the Crank-Nicolson scheme on a grid with
#  nx nodes and spacing hx and solves the tridiagonal discretized system by
#  LU factorization. Uses complex arithmetic.
#----------------------------------------------------------------------------
   a = [complex(0,0)]*(nx+1)                # diagonals of discretized system
   b = [complex(0,0)]*(nx+1)
   c = [complex(0,0)]*(nx+1)

   lam = ht/(4e0*hx*hx)

   b[1] = 1e0; c[1] = 0e0          # build coefficients of discretized system
   Psii = Psi[1]
   for i in range (2,nx):
      Psi1 = Psii; Psii = Psi[i]            # save initial wave packet values
      W = 2e0*lam + 0.5e0*ht*V[i]
      a[i] = -lam
      b[i] = complex(W,-1e0)
      c[i] = -lam
      Psi[i] = lam*Psi1 - complex(W,1e0)*Psii + lam*Psi[i+1]  # constant term

   a[nx] = 0e0; b[nx] = 1e0
                                       # solve tridiagonal discretized system
   TriDiagSys(a,b,c,Psi,nx)            # solution Psi: propagated wave packet

#============================================================================
def PropagQGS(Psi, Chi, V, nx, hx, ht):
#----------------------------------------------------------------------------
#  Propagates the solution PSI = Psi + i Chi of the 1D Schrodinger equation
#    i d/dt PSI(x,t) = [-(1/2) d2/dx2 - V(x)] PSI(x,t),
#  over the time interval ht. Uses the Crank-Nicolson scheme on a grid with
#  nx nodes and spacing hx and solves the discretized system by Gauss-Seidel
#  iterations. Uses real arithmetic.
#----------------------------------------------------------------------------
   eps = 1e-6                                            # relative tolerance
   itmax = 100                                        # max no. of iterations

   Psi0 = [0]*(nx+1); Chi0 = [0]*(nx+1); W = [0]*(nx+1)

   lam = ht/(4e0*hx*hx)

   for i in range(2,nx):                  # explicit 0th-order approximations
      W[i] = 0.5e0*ht*V[i] + 2e0*lam
      Psi0[i] = Psi[i] - lam*(Chi[i-1] + Chi[i+1]) + W[i]*Chi[i]
      Chi0[i] = Chi[i] + lam*(Psi[i-1] + Psi[i+1]) - W[i]*Psi[i]

   for it in range(1,itmax+1):                  # Gauss-Seidel iteration loop
      err = 0e0
      for i in range (2,nx):
         Psii = Psi0[i] - lam*(Chi[i-1] + Chi[i+1]) + W[i]*Chi[i]
         Chii = Chi0[i] + lam*(Psi[i-1] + Psi[i+1]) - W[i]*Psii
                          # local error estimate based on probability density
         erri = fabs((Psii*Psii+Chii*Chii) - (Psi[i]*Psi[i]+Chi[i]*Chi[i]))
         if (erri > err): err = erri                 # maximum error estimate
         Psi[i] = Psii
         Chi[i] = Chii

      if (err <= eps): break                               # convergence test

   if (it > itmax):
      print("PropagQGS: max. number of iterations exceeded !"); return 1
   return 0

#============================================================================
def PropagWave(u0, u1, u, nx, c, hx, ht):
#----------------------------------------------------------------------------
#  Propagates the solutions u0[] and u1[] of the wave equation
#     d2u/dt2 = c^2 d2u/dx2,  c - phase velocity (constant)
#  over the time interval ht, using the explicit difference scheme on a
#  spatial grid with nx nodes and spacing hx. Returns the solution in u[].
#----------------------------------------------------------------------------
   lam = c*ht/hx; lam = lam*lam
   lam2 = 2e0*(1e0 - lam)

   u[1] = u0[1]; u[nx] = u0[nx]               # Dirichlet boundary conditions
   for i in range(2,nx):              # propagate solution at interior points
      u[i] = lam*u1[i-1] + lam2*u1[i] + lam*u1[i+1] - u0[i]

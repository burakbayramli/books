#---------------------------------- ode.py ----------------------------------
#  Contains routines for solving systems of ordinary differential equations.
#  Part of the numxlib numerics library. Author: Titus Beu, 2013
#----------------------------------------------------------------------------
from math import *
from linsys import *

#============================================================================
def Euler(t, ht, y, n, Func):
#----------------------------------------------------------------------------
#  Propagates the solution y of a system of 1st order ODEs
#     y'[i] = f[i](t,y[]), i = 1..n
#  from t to t+ht using Euler's method
#  Calls: Func(t, y, f) - RHS of ODEs
#----------------------------------------------------------------------------
   f = [0]*(n+1)                                                # RHS of ODEs

   Func(t,y,f)                                              # get RHS of ODEs
   for i in range(1,n+1): y[i] += ht * f[i]              # propagate solution

#============================================================================
def EulerPC(t, ht, y, n, Func):
#----------------------------------------------------------------------------
#  Propagates the solution y of a system of 1st order ODEs
#     y'[i] = f[i](t,y[]), i = 1..n
#  from t to t+ht using Euler's predictor-corrector method
#  Calls: Func(t, y, f) - RHS of ODEs
#----------------------------------------------------------------------------
   f1 = [0]*(n+1); f2 = [0]*(n+1)                               # RHS of ODEs
   yt = [0]*(n+1)                                        # predicted solution

   Func(t,y,f1)                                            # RHS of ODEs at t
   for i in range(1,n+1): yt[i] = y[i] + ht * f1[i]               # predictor
   Func(t+ht,yt,f2)                                     # RHS of ODEs at t+ht

   ht2 = ht/2e0
   for i in range(1,n+1): y[i] += ht2 * (f1[i] + f2[i])           # corrector

#============================================================================
def RungeKutta(t, ht, y, n, Func):
#----------------------------------------------------------------------------
#  Propagates the solution y of a system of 1st order ODEs
#     y'[i] = f[i](t,y[]), i = 1..n
#  from t to t+ht using the 4th order Runge-Kutta method
#  Calls: Func(t, y, f) - RHS of ODEs
#----------------------------------------------------------------------------
   f1 = [0]*(n+1); f2 = [0]*(n+1)                               # RHS of ODEs
   f3 = [0]*(n+1); f4 = [0]*(n+1)
   yt = [0]*(n+1)                                        # predicted solution

   ht2 = ht/2e0
   Func(t,y,f1)                                                    # RHS at t
   for i in range(1,n+1): yt[i] = y[i] + ht2*f1[i]
   Func(t+ht2,yt,f2)                                          # RHS at t+ht/2
   for i in range(1,n+1): yt[i] = y[i] + ht2*f2[i]
   Func(t+ht2,yt,f3)                                          # RHS at t+ht/2
   for i in range(1,n+1): yt[i] = y[i] + ht *f3[i]
   Func(t+ht,yt,f4)                                             # RHS at t+ht

   h6 = ht/6e0                                           # propagate solution
   for i in range(1,n+1): y[i] += h6*(f1[i] + 2*(f2[i] + f3[i]) + f4[i])

#============================================================================
def RKadapt(t, ht, eps, y, n, Func):
#----------------------------------------------------------------------------
#  Propagates the solution y of a system of 1st order ODEs
#     y'[i] = f[i](t,y[]), i = 1..n
#  from t to t+ht using 4th order Runge-Kutta and adaptive step size control
#
#  ht   - initial step size (input); final step size (output):
#         ht is unchanged if err <= eps; ht is reduced if err > eps
#  ht1  - step size guess for next propagation (output)
#  eps  - max. relative error of solution components
#  Calls: Func(t, y, f) - RHS of ODEs
#----------------------------------------------------------------------------
   p = 4                                          # order of basic ODE solver
   itmax = 10                              # max. no. of step size reductions

   yt = [0]*(n+1); yt2 = [0]*(n+1)

   for it in range(1,itmax+1):                    # loop of step size scaling
      ht2 = ht/2e0
      for i in range(1,n+1): yt2[i] = yt[i] = y[i]    # initialize trial sol.
      RungeKutta(t,ht,yt,n,Func)                                  # t -> t+ht
      RungeKutta(t,ht2,yt2,n,Func)                              # t -> t+ht/2
      RungeKutta(t+ht2,ht2,yt2,n,Func)                       # t+ht/2 -> t+ht

      err = 0e0                           # max. error of solution components
      for i in range(1,n+1):
         erri = abs(1e0 - yt[i]/yt2[i]) if yt2[i] else abs(yt2[i] - yt[i])
         if (err < erri): err = erri

      f = 1e0                                  # scaling factor for step size
      if (err): f = 0.9e0*pow(eps/err,1e0/p)
      if (f > 5e0): f = 5e0                           # prevent increase > 5x
      ht1 = f * ht                                 # guess for next step size
      if (err <= eps): break                       # precision attained: exit
      ht = ht1                                  # reduce step size and repeat

   if (it > itmax): print("RKadapt: max. no. of iterations exceeded !")
   for i in range(1,n+1): y[i] = yt2[i]     # update y with the best solution
   return (ht, ht1)

#============================================================================
def RKFehlberg(t, ht, eps, y, n, Func):
#----------------------------------------------------------------------------
#  Propagates the solution y of a system of 1st order ODEs
#     y'[i] = f[i](t,y[]), i = 1..n
#  from t to t+ht using the Runge-Kutta-Fehlberg method with stepsize control
#
#  ht   - initial step size (input); final step size (output):
#         ht is unchanged if err <= eps; ht is reduced if err > eps
#  ht1  - step size guess for next propagation (output)
#  eps  - max. relative error of solution components
#  Calls: Func(t, y, f) - RHS of ODEs
#----------------------------------------------------------------------------
   p = 5                                          # order of basic ODE solver
   itmax = 10                              # max. no. of step size reductions
                                          # Runge-Kutta-Fehlberg coefficients
   a2 = 1e0/4e0; a3 = 3e0/8e0; a4 = 12e0/13e0; a5 = 1e0; a6 = 1e0/2e0
   b21 = 1e0/4e0; b31 = 3e0/32e0; b32 = 9e0/32e0
   b41 = 1932e0/2197e0; b42 = -7200e0/2197e0; b43 = 7296e0/2197e0
   b51 = 439e0/216e0; b52 = -8e0; b53 = 3680e0/513e0; b54 = -845e0/4104e0
   b61 = -8e0/27e0; b62 = 2e0; b63 = -3544e0/2565e0; b64 = 1859e0/4104e0
   b65 = -11e0/40e0
   c1 = 16e0/135e0; c3 = 6656e0/12825e0; c4 = 28561e0/56430e0
   c5 = -9e0/50e0; c6 = 2e0/55e0
   e1 = 1e0/360e0; e3 = -128e0/4275e0; e4 = -2197e0/75240e0
   e5 = 1e0/50e0; e6 = 2e0/55e0

   f1 = [0]*(n+1); f2 = [0]*(n+1); f3 = [0]*(n+1); f4 = [0]*(n+1)
   f5 = [0]*(n+1); f6 = [0]*(n+1); yt = [0]*(n+1)

   for it in range(1,itmax+1):                    # loop of step size scaling
      Func(t,y,f1)
      for i in range(1,n+1):
         yt[i] = y[i] + ht*b21*f1[i]
      Func(t+a2*ht,yt,f2)
      for i in range(1,n+1):
         yt[i] = y[i] + ht*(b31*f1[i] + b32*f2[i])
      Func(t+a3*ht,yt,f3)
      for i in range(1,n+1):
         yt[i] = y[i] + ht*(b41*f1[i] + b42*f2[i] + b43*f3[i])
      Func(t+a4*ht,yt,f4)
      for i in range(1,n+1):
         yt[i] = y[i] + ht*(b51*f1[i] + b52*f2[i] + b53*f3[i] + b54*f4[i])
      Func(t+a5*ht,yt,f5)
      for i in range(1,n+1):
         yt[i] = y[i] + ht*(b61*f1[i] + b62*f2[i] + b63*f3[i] + b64*f4[i] + \
                            b65*f5[i])
      Func(t+a6*ht,yt,f6)

      err = 0e0                           # max. error of solution components
      for i in range(1,n+1):                        # O(h5) solution estimate
         yt[i] = y[i] + \
                 ht*(c1*f1[i] + c3*f3[i] + c4*f4[i] + c5*f5[i] + c6*f6[i])
                                                             # error estimate
         erri = ht*(e1*f1[i] + e3*f3[i] + e4*f4[i] + e5*f5[i] + e6*f6[i])
         erri = fabs(erri/yt[i])
         if (err < erri): err = erri

      f = 1e0                                  # scaling factor for step size
      if (err): f = 0.9e0*pow(eps/err,1e0/p)
      if (f > 5e0): f = 5e0                           # prevent increase > 5x
      ht1 = f * ht                                 # guess for next step size
      if (err <= eps): break                       # precision attained: exit
      ht = ht1                                  # reduce step size and repeat

   if (it > itmax): print("RKFehlberg: max. no. of iterations exceeded !")
   for i in range(1,n+1): y[i] = yt[i]      # update y with the best solution
   return (ht, ht1)

#============================================================================
def Euler1(t, ht, y, dy, Func):
#----------------------------------------------------------------------------
#  Propagates the solution y and 1st derivative dy of a 2nd order ODE from t
#  to t+ht using Euler's method
#----------------------------------------------------------------------------
   d2y = Func(t,y,dy)                                              # d2y -> t

   y  += ht * dy                                                  # y -> t+ht
   dy += ht * d2y                                                # dy -> t+ht
   return (y, dy)

#============================================================================
def EulerCromer1(t, ht, y, dy, Func):
#----------------------------------------------------------------------------
#  Propagates the solution y and the 1st derivative dy of a 2nd order ODE
#  from t to t+ht using the Euler-Cromer method
#----------------------------------------------------------------------------
   d2y = Func(t,y,dy)                                              # d2y -> t

   dy += ht * d2y                                                # dy -> t+ht
   y  += ht * dy                                                  # y -> t+ht
   return (y, dy)

#============================================================================
def Verlet1(t, ht, y, dy, d2y, Func):
#----------------------------------------------------------------------------
#  Propagates the solution y and the 1st derivative dy of a 2nd order ODE
#  from t to t+ht using the Verlet method; returns 2nd derivative in d2y;
#  d2y needs to be initialized on first call and saved between calls
#----------------------------------------------------------------------------
   ht2 = 0.5e0 * ht
   dy += ht2 * d2y                                             # dy -> t+ht/2
   y  += ht  * dy                                                 # y -> t+ht

   d2y = Func(t,y,dy)                                           # d2y -> t+ht

   dy += ht2 * d2y                                               # dy -> t+ht
   return (y, dy, d2y)

#============================================================================
def Euler2(t, ht, y, dy, n, Func):
#----------------------------------------------------------------------------
#  Propagates the solution y and the 1st derivative dy of a system of n
#  2nd order ODEs from t to t+ht using the Euler method
#----------------------------------------------------------------------------
   d2y = [0]*(n+1)

   Func(t,y,dy,d2y)                                                # d2y -> t

   for i in range(1,n+1):                                # propagate solution
      y[i]  += ht * dy[i]                                         # y -> t+ht
      dy[i] += ht * d2y[i]                                       # dy -> t+ht

#============================================================================
def EulerCromer(t, ht, y, dy, n, Func):
#----------------------------------------------------------------------------
#  Propagates the solution y and the 1st derivative dy of a system of n
#  2nd order ODEs from t to t+ht using the Euler-Cromer method
#----------------------------------------------------------------------------
   d2y = [0]*(n+1)

   Func(t,y,dy,d2y)                                                # d2y -> t

   for i in range(1,n+1):                                # propagate solution
      dy[i] += ht * d2y[i]                                       # dy -> t+ht
      y[i]  += ht * dy[i]                                         # y -> t+ht

#============================================================================
def Verlet2(ht, m, x, y, vx, vy, ax, ay, Forces):
#----------------------------------------------------------------------------
#  Propagates the 2D solution of Newton's equations of motion for a particle
#  of mass m over a time interval ht using the velocity Verlet method
#  x, y   - position components
#  vx, vy - velocity components
#  ax, ay - acceleration components (need to be initialized on 1st call)
#  Ekin   - kinetic energy
#  Epot   - potential energy
#----------------------------------------------------------------------------
   ht2 = 0.5e0 * ht
   vx += ht2 * ax; x += ht * vx                                  # v -> t+h/2
   vy += ht2 * ay; y += ht * vy                                    # r -> t+h

   (fx, fy, Epot) = Forces(m,x,y,vx,vy)                              # forces

   ax = fx/m; ay = fy/m                                            # a -> t+h
   vx += ht2 * ax                                                  # v -> t+h
   vy += ht2 * ay

   Ekin = 0.5e0 * m * (vx*vx + vy*vy)                        # kinetic energy

   return (x, y, vx, vy, ax, ay, Ekin, Epot)

#============================================================================
def Verlet(ht, m, x, y, z, vx, vy, vz, ax, ay, az, n, Forces):
#----------------------------------------------------------------------------
#  Propagates the solution of Newton's equations of motion for a system of n
#  particles over a time interval ht using the velocity Verlet method
#  m          - masses of particles
#  x, y, z    - positions
#  vx, vy, vz - velocities
#  ax, ay, az - accelerations (need to be initialized on 1st call)
#  Ekin       - total kinetic energy
#  Epot       - total potential energy
#----------------------------------------------------------------------------
   ht2 = 0.5e0 * ht
   for i in range(1,n+1):
      vx[i] += ht2 * ax[i]; x[i] += ht * vx[i]                   # v -> t+h/2
      vy[i] += ht2 * ay[i]; y[i] += ht * vy[i]                     # r -> t+h
      vz[i] += ht2 * az[i]; z[i] += ht * vz[i]

   Epot = Forces(m,x,y,z,ax,ay,az,n)                                 # forces

   Ekin = 0e0
   for i in range(1,n+1):                                    # corrector step
      ax[i] /= m[i]; ay[i] /= m[i]; az[i] /= m[i]                 # a -> t+ht
      vx[i] += ht2 * ax[i]                                        # v -> t+ht
      vy[i] += ht2 * ay[i]
      vz[i] += ht2 * az[i]

      Ekin += 0.5e0 * m[i] * (vx[i]*vx[i] + vy[i]*vy[i] + vz[i]*vz[i])

   return (Ekin, Epot)

#============================================================================
def EulerCromerQM(E, V, x, y, nx, y0, dy0):
#----------------------------------------------------------------------------
#  Propagates the solution of the dimensionless 1D Schrodinger equation
#     y" = 2 [V(x) - E] y, y(0) = y0, y'(0) = y'0
#  on a regular mesh x[] with nx points by the Euler-Cromer method. Receives
#  the energy in E, the tabulated potential in V[], the initial conditions in
#  y0 and dy0. Returns the index of the divergence point (default is nx).
#----------------------------------------------------------------------------
   hx = x[2] - x[1]                                   # propagation step size
   y[1] = y0; dy = dy0                                       # initial values
   for m in range(2,nx):                                   # propagation loop
      d2y = 2e0 * (V[m] - E) * y[m]             # RHS of Schrodinger equation
      dy += hx * d2y                                          # dy -> x[m]+hx
      y[m+1] = y[m] + hx * dy                                  # y -> x[m]+hx

      if (abs(y[m+1]) > 1e10): break                     # stop if y diverges

   return m                                       # index of divergence point

#============================================================================
def Numerov(E, V, x, y, nx, y0, dy0):
#----------------------------------------------------------------------------
#  Propagates the solution of the dimensionless 1D Schrodinger equation
#     y" = 2 [V(x) - E] y, y(0) = y0, y'(0) = y'0
#  on a regular mesh x[] with nx points by the Numerov method. Receives the
#  energy in E, the tabulated potential in V[], and the initial conditions in
#  y0 and dy0. Returns the index of the divergence point (default is nx).
#----------------------------------------------------------------------------
   hx = x[2] - x[1]                                   # propagation step size
   y[1] = y0; dy = dy0                                       # initial values

   d2y = 2e0 * (V[1] - E) * y[1]                  # initial Euler-Cromer step
   dy += hx * d2y
   y[2] = y[1] + hx * dy

   h6 = hx*hx/6e0
   um1 = 1e0 - h6 * (V[1] - E)                    # stack of auxiliary values
   um  = 1e0 - h6 * (V[2] - E)
   for m in range(2,nx):
      up1 = 1e0 - h6 * (V[m+1] - E)
      y[m+1] = ((12e0 - 10e0*um) * y[m] - um1 * y[m-1]) / up1
      um1 = um; um = up1                     # shift stack down for next step

      if (abs(y[m+1]) > 1e10): break                     # stop if y diverges

   return m                                       # index of divergence point

#============================================================================
def Propag(x, y, nx, y0, dy0, Func):
#----------------------------------------------------------------------------
#  Propagates the solution y[] of a Cauchy-problem for a 2nd order ODE on a
#  regular mesh x[] with nx points, starting from y0 and dy0.
#  Calls: EulerCromer1(x, hx, y, dy, Func); Func(x, y, dy) - RHS of ODE
#----------------------------------------------------------------------------
   hx = x[2] - x[1]
   y[1] = y0; dy = dy0
   for m in range(1,nx):
      (y[m+1], dy) = EulerCromer1(x[m],hx,y[m],dy,Func)

#============================================================================
def Shoot(x, y, nx, ya, yb, dy1, dy2, eps, Func):
#----------------------------------------------------------------------------
#  Solves a two-point boundary-value problem for a 2nd order ODE
#     y" = f(x,y,y'), y(xa) = ya, y(xb) = yb
#  on a regular mesh x[] with nx points, using the shooting method with trial
#  initial derivatives dy in [dy1,dy2]. Returns the solution y[] satisfying
#  the condition y(xb) = yb within tolerance eps, the used derivative dy, and
#  an existence flag.
#  Calls: Propag(x, y, nx, y0, dy0, Func); Func(x, y, dy) - RHS of ODE
#----------------------------------------------------------------------------
   itmax = 100                                    # max. number of bisections

   Propag(x,y,nx,ya,dy1,Func)                           # propagate y for dy1
   f1 = y[nx] - yb                                          # deviation at xb
   Propag(x,y,nx,ya,dy2,Func)                           # propagate y for dy2
   f2 = y[nx] - yb                                          # deviation at xb

   if (f1*f2 < 0):                          # check if dy exists in [dy1,dy2]
      exist = 1
      for it in range(1,itmax+1):                    # refine dy by bisection
         dy = 0.5e0*(dy1 + dy2)                           # new approximation
         Propag(x,y,nx,ya,dy,Func)                              # propagate y
         f = y[nx] - yb                                     # deviation at xb
         if (f1*f > 0): dy1 = dy                          # new semi interval
         else:          dy2 = dy
         if (fabs(f) <= eps): break              # deviation vanishes at xb ?

      if (it >= itmax): print("Shoot: max. no. of bisections exceeded !")
   else:
      dy = 1e10; exist = 0

   return (dy, exist)

#============================================================================
def ShootQM(E1, E2, V, x, y, nx, nc, y0, dy0, eps):
#----------------------------------------------------------------------------
#  Solves the two-point eigenvalue problem for the 1D Schrodinger equation
#     y" = (2m/h^2) [V(x) - E] y, y(0) = y0, y(+inf) = 0
#  on a regular mesh x[] with nx points, using the shooting method with the
#  trial energies E in [E1,E2] and the tabulated potential in V[]. Returns
#  the solution y[] vanishing at the checkpoint x[nc] within tolerance eps,
#  and the existence flag exist.
#  Calls: Numerov(V, x, y, nx, y0, dy0, E)
#----------------------------------------------------------------------------
   itmax = 100                                    # max. number of bisections

   inf = Numerov(E1,V,x,y,nx,y0,dy0)                     # propagate y for E1
   f1 = y[inf]                                                 # asymptotic y
   inf = Numerov(E2,V,x,y,nx,y0,dy0)                     # propagate y for E2
   f2 = y[inf]                                                 # asymptotic y

   if (f1*f2 < 0):                             # check if exists E in [E1,E2]
      exist = 1
      for it in range(1,itmax+1):                     # refine E by bisection
         E = 0.5e0*(E1 + E2)                              # new approximation
         inf = Numerov(E,V,x,y,nx,y0,dy0)                       # propagate y
         f = y[inf]                                            # asymptotic y
         if (f1*f > 0): E1 = E                            # new semi interval
         else:          E2 = E
         if (fabs(y[nc]) <= eps): break      # check if y vanishes at x[nc] ?

      if (it >= itmax): print("ShootQM: max. no. of bisections exceeded !")
   else:
      E = 1e10; exist = 0

   return (E, exist)

#============================================================================
def Bilocal(xa, xb, y, nx, alf1, bet1, alf2, bet2, Func):
#----------------------------------------------------------------------------
#  Solves a linear two-point boundary-value problem for a 2nd order ODE
#     y" = p(x) y' + q(x) y + r(x),  xa <= x <= xb
#     alf1 y(xa) + bet1 y'(xa) = 1
#     alf2 y(xa) + bet2 y'(xa) = 1
#  on a regular mesh with nx points, using central finite-differences.
#  Returns the solution in y[].
#  Calls: Func(x, p, q, r) - returns values of p(x), q(x), r(x)
#         TriDiagSys (linsys.py) - solves discretized tridiagonal system
#----------------------------------------------------------------------------
   a = [0]*(nx+1); b = [0]*(nx+1); c = [0]*(nx+1)           # matrix elements

   hx = (xb-xa)/(nx-1); h2 = 2e0*hx*hx
                                                  # build system coefficients
   b[1] = hx*alf1 - bet1; c[1] = bet1; y[1] = hx
   for m in range (2,nx):
      x = xa + (m-1)*hx                                          # mesh point
      (p,q,r) = Func(x)
      a[m] = -(2e0 + hx*p); b[m] = 4e0 + h2*q; c[m] = -(2e0 - hx*p)
      y[m] = -h2*r                               # RHSs of tridiagonal system
   a[nx] = -bet2; b[nx] = hx*alf2 + bet2; y[nx] = hx

   TriDiagSys(a,b,c,y,nx)                          # solve discretized system

#--------------------------------- roots.py ---------------------------------
#  Contains routines for determining real roots of real functions.
#  Part of the numxlib numerics library. Author: Titus Beu, 2013
#----------------------------------------------------------------------------
from math import *
from linsys import *

#============================================================================
def Bisect(Func, a, b):
#----------------------------------------------------------------------------
#  Determines a real root x of function Func isolated in interval [a,b] by
#  the bisection method
#  Error code: 0 - normal execution
#              1 - [a,b] does not isolate one root or contains several roots
#              2 - max. number of iterations exceeded
#----------------------------------------------------------------------------
   eps = 1e-10                                   # relative precision of root
   itmax = 100                                       # max. no. of iterations

   x = a; fa = Func(x)                                       # is a the root?
   if (fabs(fa) == 0e0): return (x,0)
   x = b; fb = Func(x)                                       # is b the root?
   if (fabs(fb) == 0e0): return (x,0)

   if (fa*fb > 0): return (x,1)               # [a,b] does not contain a root
                                              # or contains several roots
   for it in range(1,itmax+1):
      x = 0.5e0 * (a + b)                                 # new approximation
      fx = Func(x)
      if (fa*fx > 0): a = x                    # choose new bounding interval
      else: b = x
      if (((b-a) <= eps*fabs(x)) or (fabs(fx) <= eps)): return (x,0)

   print("Bisect: max. no. of iterations exceeded !"); return (x,2)

#============================================================================
def FalsPos(Func, a, b):
#----------------------------------------------------------------------------
#  Determines a real root x of function Func isolated in interval [a,b] by
#  the false position method
#  Error code: 0 - normal execution
#              1 - [a,b] does not isolate one root or contains several roots
#              2 - max. number of iterations exceeded
#----------------------------------------------------------------------------
   eps = 1e-10                                   # relative precision of root
   itmax = 100                                       # max. no. of iterations

   x = a; fa = Func(x)                                       # is a the root?
   if (fabs(fa) == 0e0): return (x,0)
   x = b; fb = Func(x)                                       # is b the root?
   if (fabs(fb) == 0e0): return (x,0)

   if (fa*fb > 0): return (x,1)               # [a,b] does not contain a root
                                              # or contains several roots
   for it in range(1,itmax+1):
      x = (a*fb - b*fa)/(fb - fa)                         # new approximation
      fx = Func(x)
      if (fa*fx > 0):                          # choose new bounding interval
         dx = x - a; a = x; fa = fx
      else:
         dx = b - x; b = x; fb = fx
      if ((fabs(dx) <= eps*fabs(x)) or (fabs(fx) <= eps)): return (x,0)

   printf("FalsPos: max. no. of iterations exceeded !"); return (x,2)

#============================================================================
def Iter(Func, a, b, x):
#----------------------------------------------------------------------------
#  Determines a root x of function Func isolated in [a,b] by the method of
#  successive approximations. x contains on entry an initial approximation.
#  Error code: 0 - normal execution
#              1 - interval does not contain a root
#              2 - max. number of iterations exceeded
#              3 - diverging process
#----------------------------------------------------------------------------
   eps = 1e-10                                   # relative precision of root
   itmax = 100                                       # max. no. of iterations

   dx = -Func(x)                                      # initialize correction
   for it in range(1,itmax+1):
      f = Func(x)
      if (fabs(f) > fabs(dx)): break        # compare new with old correction
      dx = -f                                             # update correction
      x += dx                                             # new approximation
      if ((x < a) or (x > b)): return (x,1)   # [a,b] does not contain a root
      if (fabs(dx) <= eps*fabs(x)): return (x,0)          # check convergence

   if (it > itmax):
      print("Iter: max. no. of iterations exceeded !"); return (x,2)
   if (fabs(f) > fabs(dx)):
      print("Iter: diverging process !"); return (x,3)

#============================================================================
def Newton(Func, a, b, x):
#----------------------------------------------------------------------------
#  Determines a real root x of function Func isolated in interval [a,b] by
#  the Newton-Raphson method using the analytical derivative. x contains on
#  input an initial approximation.
#  Error code: 0 - normal execution
#              1 - interval does not contain a root
#              2 - max. number of iterations exceeded
#----------------------------------------------------------------------------
   eps = 1e-10                                   # relative precision of root
   itmax = 100                                       # max. no. of iterations

   for it in range(1,itmax+1):
      (f,df) = Func(x)                              # function and derivative
      dx = -f/df if fabs(df) > eps else -f                  # root correction
      x += dx                                             # new approximation
      if ((x < a) or (x > b)): return (x,1)   # [a,b] does not contain a root
      if (fabs(dx) <= eps*fabs(x)): return (x,0)          # check convergence

   print("Newton: max. no. of iterations exceeded !"); return (x,2)

#============================================================================
def NewtonNumDrv(Func, a, b, x):
#----------------------------------------------------------------------------
#  Determines a real root x of function Func isolated in interval [a,b] by
#  the Newton-Raphson method using the numerical derivative. x contains on
#  input an initial approximation.
#  Error code: 0 - normal execution
#              1 - interval does not contain a root
#              2 - max. number of iterations exceeded
#----------------------------------------------------------------------------
   eps = 1e-10                                   # relative precision of root
   itmax = 100                                       # max. no. of iterations

   for it in range(1,itmax+1):
      f = Func(x)
      dx = eps*fabs(x) if x else eps                        # derivation step
      df = (Func(x+dx)-f)/dx                           # numerical derivative
      dx = -f/df if fabs(df) > eps else -f                  # root correction
      x += dx                                             # new approximation
      if ((x < a) or (x > b)): return (x,1)   # [a,b] does not contain a root
      if (fabs(dx) <= eps*fabs(x)): return (x,0)          # check convergence

   print("NewtonNumDrv: max. no. of iterations exceeded !"); return (x,2)

#============================================================================
def Secant(Func, a, b, x):
#----------------------------------------------------------------------------
#  Determines a real root x of function Func isolated in interval [a,b] by
#  the secant method. x contains on entry an initial approximation.
#  Error code: 0 - normal execution
#              1 - interval does not contain a root
#              2 - max. number of iterations exceeded
#----------------------------------------------------------------------------
   eps = 1e-10                                   # relative precision of root
   itmax = 1000                                      # max. no. of iterations

   x0 = x; f0 = Func(x0)
   x = x0 - f0                                          # first approximation
   for it in range(1,itmax+1):
      f = Func(x)
      df = (f-f0)/(x-x0)                             # approximate derivative
      x0 = x; f0 = f                      # store abscissa and function value
      dx = -f/df if fabs(df) > eps else -f                  # root correction
      x += dx                                             # new approximation
      if ((x < a) or (x > b)): return (x,1)   # [a,b] does not contain a root
      if (fabs(dx) <= eps*fabs(x)): return (x,0)          # check convergence

   print("Secant: max. no. of iterations exceeded !"); return (x,2)

#============================================================================
def BirgeVieta(a, n, xx):
#----------------------------------------------------------------------------
#  Determines the real roots of a real polynomial by the Birge-Vieta method
#  a[]   - coefficients of the polynomial
#  n     - order of the polynomial
#  xx[]  - array of roots (output)
#  nx    - number of found roots (output)
#----------------------------------------------------------------------------
   eps = 1e-10                                   # relative precision of root
   itmax = 100                                       # max. no. of iterations

   nx = 0
   if (n <= 1): return

   x = 0                                 # initial approximation for 1st root
   for m in range(n,1,-1):                    # loop over reduced polynomials
      for it in range(1,itmax+1):                     # Newton-Raphson method
         p = a[0]; d = p                                    # Horner's scheme
         for j in range(1,m):
            p = p*x + a[j]                                       # polynomial
            d = d*x + p                                          # derivative
         p = p*x + a[m]
         d = -p/d if d else -p                              # root correction
         x += d
         if (fabs(d) <= eps*fabs(x)): break               # check convergence

      if (it == itmax):
         print("Birge: max. no. of iterations exceeded !"); return

      nx += 1
      xx[nx] = x                                                 # store root
                                     # coefficients of new reduced polynomial
      for j in range(1,m): a[j] += a[j-1]*x

   nx += 1
   xx[nx] = -a[1]/a[0]                         # root of 1st order polynomial

   return nx

#============================================================================
def Jacobian(x, jac, n, Func):
#----------------------------------------------------------------------------
#  Calculates the Jacobian of a system of n real functions with n variables
#  using central finite differences
#  x[]     - point at which the Jacobian is evaluated
#  jac[][] - Jacobian
#  n       - space dimension
#  Func    - user function returning the function values at point x
#               Func(f, x, n)
#----------------------------------------------------------------------------
   eps = 1e-10

   fm = [0]*(n+1)
   fp = [0]*(n+1)

   for j in range(1,n+1):                             # loop over coordinates
      x0 = x[j]                                                  # store x[j]
      h = eps*fabs(x0) if x0 else eps                             # step-size
      x[j] = x0 - h; Func(fm,x,n)                            # decrement x[j]
      x[j] = x0 + h; Func(fp,x,n)                            # increment x[j]
      h2 = 1e0/(2e0*h)
      for i in range(1,n+1): jac[i][j] = (fp[i] - fm[i]) * h2      # Jacobian
      x[j] = x0                                                # restore x[j]

#============================================================================
def NewtonSys(Func, x, n):
#----------------------------------------------------------------------------
#  Determines a n-dimensional real zero of a system of n real functions by
#  Newton-Raphson method.
#  Func - user function returning the function values f[] for arguments x[]
#            Func(f, x, n)
#  x[]  - initial approximation (input), solution (output)
#  dx[] - error estimates of the solution components (output)
#  n    - order of system
#  ierr - error code: 0 - normal execution
#                     1 - max. number of iterations exceeded
#  Calls: Jacobian - computes Jacobian
#         MatInv   - inverts (n x n) matrix (in linsys.h)
#----------------------------------------------------------------------------
   eps = 1e-14                                # precision for cumulated error
   itmax = 200                                       # max. no. of iterations

   f   = [0]*(n+1)
   dx  = [0]*(n+1)
   jac = [[0]*(n+1) for i in range(n+1)]

   for it in range(1,itmax+1):
      Func(f,x,n)                                                 # functions
      Jacobian(x,jac,n,Func)                                       # Jacobian
      det = MatInv(jac,n)                                  # inverse Jacobian

      if (det):                                                 # corrections
         for i in range(1,n+1):                       # non-singular Jacobian
            sum = 0e0
            for j in range(1,n+1): sum -= jac[i][j] * f[j]
            dx[i] = sum
      else:
         for i in range(1,n+1): dx[i] = -f[i]             # singular Jacobian

      err = 0e0
      for i in range(1,n+1):
         x[i] += dx[i]                                    # new approximation
         err += fabs(f[i])                                   # cumulate error
      if (err <= eps): break                              # check convergence

   ierr = 0
   if (it >= itmax):
      ierr = 1; print("NewtonSys: max. no. of iterations exceeded !")
   return (dx,ierr)

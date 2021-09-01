#------------------------------- specfunc.py --------------------------------
#  Contains routines for evaluating special functions.
#  Part of the numxlib numerics library. Author: Titus Beu, 2013
#----------------------------------------------------------------------------
from math import *

#============================================================================
def Chebyshev(n, x):
#----------------------------------------------------------------------------
#  Evaluates the n-th order Chebyshev polynomial and its derivative d in x
#  using the recurrence relation
#----------------------------------------------------------------------------
   if (n == 0):
      f = 1e0; d = 0e0
   else:
      f = x; fm1 = 1e0; x2 = 2*x
      for i in range(2,n+1):
         fm2 = fm1; fm1 = f
         f = x2*fm1 - fm2

      d = n*(x*f-fm1)/(x*x-1e0) if (x*x-1e0) else n*n*f/x

   return (f, d)

#============================================================================
def Legendre(n, x):
#----------------------------------------------------------------------------
#  Evaluates the n-th order Legendre polynomial and its derivative d in x
#  using the recurrence relation
#----------------------------------------------------------------------------
   if (n == 0):
      f = 1e0; d = 0e0
   else:
      f = x; fm1 = 1e0
      for i in range(2,n+1):
         fm2 = fm1; fm1 = f
         f = ((2*i-1)*x*fm1 - (i-1)*fm2)/i

      d = n*(x*f-fm1)/(x*x-1e0) if (x*x-1e0) else 0.5*n*(n+1)*f/x

   return (f, d)

#============================================================================
def aLegendre(l, m, x):
#----------------------------------------------------------------------------
#  Evaluates the associated Legendre function of orders l and m >= 0 in x
#----------------------------------------------------------------------------
   if (l < m): return 0e0

   p = 1e0; pm1 = 0e0                     # seed values: P(m,m,x), P(m-1,m,x)
   if (m):
      sqx = -sqrt(1e0-x*x)
      for i in range(1,m+1): p *= (2*i-1) * sqx

   for i in range(m+1,l+1):                                      # recurrence
      pm2 = pm1; pm1 = p
      p = ((2*i-1)*x*pm1 - (i+m-1)*pm2)/(i-m)

   return p

#============================================================================
def Laguerre(n, x):
#----------------------------------------------------------------------------
#  Evaluates the n-th order Laguerre polynomial and its derivative d in x
#  using the recurrence relation
#----------------------------------------------------------------------------
   if (n == 0):
      f = 1e0; d = 0e0
   else:
      f = 1e0 - x; fm1 = 1e0
      for i in range(2,n+1):
         fm2 = fm1; fm1 = f
         f = ((2*i-1-x)*fm1 - (i-1)*fm2)/i

      d = n*(f-fm1)/x if x else -n*f

   return (f, d)

#============================================================================
def aLaguerre(n, k, x):
#----------------------------------------------------------------------------
#  Evaluates the associated Laguerre polynomial of orders n and k in x
#  using the recurrence relation
#----------------------------------------------------------------------------
   if (n == 0):
      f = 1e0
   else:
      f = 1e0 + k - x; fm1 = 1e0
      for i in range(2,n+1):
         fm2 = fm1; fm1 = f
         f = ((2*i+k-1-x)*fm1 - (i+k-1)*fm2)/i

   return f

#============================================================================
def Hermite(n, x):
#----------------------------------------------------------------------------
#  Evaluates the n-th order Hermite polynomial and its derivative d in x
#  using the recurrence relation
#----------------------------------------------------------------------------
   if (n == 0):
      f = 1e0; d = 0e0
   else:
      f = 2*x; fm1 = 1e0; x2 = 2*x
      for i in range(2,n+1):
         fm2 = fm1; fm1 = f
         f = x2*fm1 - 2*(i-1)*fm2
      d = 2*n*fm1

   return (f, d)

#============================================================================
def SpherY(l, m, theta, phi):
#----------------------------------------------------------------------------
#  Evaluates the real and imaginary parts (ReY and ImY) of the spherical
#  harmonic of orders l and m for arguments theta and phi.
#  Calls aLegendre to calculate associated Legendre polynomials.
#----------------------------------------------------------------------------
   mabs = abs(m)

   fact = 1e0
   for i in range(l-mabs+1,l+mabs+1): fact *= i           # (l+|m|)!/(l-|m|)!

   fact = sqrt((2*l+1)/(4e0*pi*fact)) * aLegendre(l,mabs,cos(theta))
   if (m < 0 and m % 2): fact = -fact

   ReY = fact * cos(m*phi)
   ImY = fact * sin(m*phi)

   return (ReY,ImY)

#============================================================================
def SBessj(n, x):
#----------------------------------------------------------------------------
#  Evaluates iteratively the spherical Bessel function of order n in x
#----------------------------------------------------------------------------
   if (x == 0e0): return 1e0 if (n == 0) else 0e0
   j0 = sin(x)/x
   if (n == 0): return j0
   j1 = (j0 - cos(x))/x
   if (n == 1): return j1

   nmax = 0                            # finds direction of stable recurrence
   if (n >= fabs(x)):                  # nmax = 0 forward, nmax /= 0 backward
      jn = 1.
      for i in range(n,n+51):
         jn *= (2*i-1)/x                             # net factor of increase
         if (jn >= 1e8): nmax = i + 10; break        # for forward iteration

   if (nmax == 0):                                        # forward iteration
      for i in range(2,n+1):
         j = (2*i-1)/x*j1 - j0
         j0 = j1; j1 = j
      return j
   else:                                                 # backward iteration
      j2 = 0.; j1 = 1e-20
      for i in range(nmax,-1,-1):
         j = (2*i+3)/x*j1 - j2
         j2 = j1; j1 = j
         if (i == n): jn = j                              # non-normalized jn

      return (j0/j)*jn                                        # normalized jn

#============================================================================
def SBessy(n, x):
#----------------------------------------------------------------------------
#  Evaluates iteratively the spherical Neumann function of order n in x
#----------------------------------------------------------------------------
   y0 = -cos(x)/x
   if (n == 0): return y0
   y1 = (y0 - sin(x))/x
   if (n == 1): return y1

   for i in range(2,n+1):
      y = (2*i-1)/x*y1 - y0
      y0 = y1; y1 = y

   return y

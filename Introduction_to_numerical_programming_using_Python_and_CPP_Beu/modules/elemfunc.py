#------------------------------- elemfunc.py --------------------------------
#  Contains routines for evaluating elementary functions.
#  Author: Titus Beu, 2013
#----------------------------------------------------------------------------
from math import *

#============================================================================
def Poly(x, a, n):
#----------------------------------------------------------------------------
#  Evaluates the polynomial P(x) = a[0] x^n + a[1] x^(n-1) + ... + a[n] with
#  real coefficients in x using Horner's scheme
#----------------------------------------------------------------------------
   p = a[0]
   for i in range(1,n+1): p = p*x + a[i]
   return p

#============================================================================
def PolyDerive(a, b, n):
#----------------------------------------------------------------------------
#  For the real polynomial P(x) = a[0] x^n + ... + a[n], the function returns
#  the coefficients of the derivative P'(x) = b[0] x^(n-1) + ... + b[n-1]
#----------------------------------------------------------------------------
   for i in range(0,n+1): b[i] = (n-i) * a[i]

#============================================================================
def PolyDivide(x0, a, b, n):
#----------------------------------------------------------------------------
#  For the real polynomial P(x) = a[0] x^n + ... + a[n], the function returns
#  the coefficients of the division by the binomial (x-x0):
#  P(x) = (x-x0) (b[0] x^(n-1) + ... + b[n-1]) + b[n] (b[n] = P(x0))
#----------------------------------------------------------------------------
   b[0] = a[0]
   for i in range(1,n+1): b[i] = b[i-1]*x0 + a[i]

#============================================================================
def Exp0(x):
#----------------------------------------------------------------------------
#  Evaluates exp(x) from its power-series expansion
#----------------------------------------------------------------------------
   eps = 1e-14                                           # relative precision
   
   i = 0
   f = t = 1e0
   while (fabs(t) > eps*fabs(f)):
      i += 1
      t *= x/i
      f += t

   return f

#============================================================================
def Exp(x):
#----------------------------------------------------------------------------
#  Evaluates exp(x) from its power-series expansion
#  For x < 0 avoids potential instabilities due to subtractions
#----------------------------------------------------------------------------
   eps = 1e-14                                           # relative precision
   
   i = 0
   f = t = 1e0
   absx = fabs(x)
   while (fabs(t) > eps*fabs(f)):
      i += 1
      t *= absx/i
      f += t

   return f if (x >= 0e0) else 1e0/f

#============================================================================
def Sin(x):
#----------------------------------------------------------------------------
#  Evaluates sin(x) from its power-series expansion
#----------------------------------------------------------------------------
   eps = 1e-14                                           # relative precision
   
   i = 1
   f = t = x
   x2 = x*x
   while (fabs(t) > eps*fabs(f)):
      i += 2
      t *= -x2/((i-1)*i)
      f += t

   return f

#============================================================================
def ArcSin(x):
#----------------------------------------------------------------------------
#  Evaluates arcsin(x) from its power-series expansion
#----------------------------------------------------------------------------
   eps = 1e-14                                           # relative precision
   
   i = 1
   f = t = x
   x2 = x*x
   while (fabs(t) > eps*fabs(f)):
      i2 = i*i
      i += 2
      t *= i2*x2/((i-1)*i)
      f += t

   return f

#============================================================================
def Tan(x):
#----------------------------------------------------------------------------
#  Evaluates tan(x) from its continued fraction representation
#----------------------------------------------------------------------------
   eps = 1e-14                                           # relative precision

   a = 1e0; b = -x*x
   pm1 = 0e0; p = x
   qm1 = 1e0; q = 1e0
   rm1 = 0e0; r = x
   while (fabs(r-rm1) > eps*fabs(r)):
      pm2 = pm1; pm1 = p                                    # shift the stack
      qm2 = qm1; qm1 = q
      rm1 = r
      a += 2e0
      p = a*pm1 + b*pm2
      q = a*qm1 + b*qm2
      r = p/q                                          # new convergent value

   return r

#============================================================================
def Exp1(x):
#----------------------------------------------------------------------------
#  Evaluates exp(x) from its continued fraction representation
#----------------------------------------------------------------------------
   eps = 1e-14                                           # relative precision

   a = 1e0; b = x
   pm1 = 1e0; p = 1e0 + x
   qm1 = 1e0; q = 1e0
   rm1 = 1e0; r = p/q
   i = 1
   while (fabs(r-rm1) > eps*fabs(r)):
      i += 1
      pm2 = pm1; pm1 = p                                    # shift the stack
      qm2 = qm1; qm1 = q
      rm1 = r
      a = (i if i%2 else 2e0)
      b = -b
      p = a*pm1 + b*pm2
      q = a*qm1 + b*qm2
      r = (p/q if q else 9e99)                         # new convergent value

   return r

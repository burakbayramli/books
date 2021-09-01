#-------------------------------- modfunc.py --------------------------------
#  Routines for modeling tabular dependences by interpolation/regression.
#  Part of the numxlib numerics library. Author: Titus Beu, 2013
#----------------------------------------------------------------------------
from linsys import *

#============================================================================
def Lagrange(x, y, n, xi):
#----------------------------------------------------------------------------
#  Evaluates the Lagrange interpolating polynomial of n data points at xi
#  x[] - x-coordinates of data points
#  y[] - y-coordinates of data points
#  n   - number of data points
#  xi  - interpolation argument
#----------------------------------------------------------------------------
   yi = 0e0
   for i in range(1,n+1):
      p = 1e0
      for j in range(1,n+1):
          if (j != i): p *= (xi - x[j])/(x[i] - x[j])
      yi += p * y[i]

   return yi

#============================================================================
def Lagrange1(x, y, n, xi, yi, ni):
#----------------------------------------------------------------------------
#  Evaluates the Lagrange interpolating polynomial of n data points on a mesh
#  of ni interpolation points
#  x[]  - x-coordinates of data points
#  y[]  - y-coordinates of data points
#  n    - number of data points
#  xi[] - interpolation arguments
#  yi[] - interpolant values (output)
#  ni   - number of interpolation points
#----------------------------------------------------------------------------
   yf = [[0]*(n+1) for i in range(n+1)]          # factors of the interpolant

   for i in range(1,n+1):      # prepare invariant factors of the interpolant
      p = 1e0
      for j in range(1,n+1):
          if (j != i): p *= (x[i] - x[j])
      yf[i] = y[i] / p

   for k in range(1,ni+1):                  #  loop over interpolation points
      xk = xi[k]; yk = 0e0
      for i in range(1,n+1):
         p = 1e0
         for j in range(1,n+1):
             if (j != i): p *= (xk - x[j])
         yk += p * yf[i]

      yi[k] = yk

#============================================================================
def Neville(x, y, n, xi):
#----------------------------------------------------------------------------
#  Evaluates the Lagrange interpolating polynomial of n data points at xi by
#  Neville's method and returns an estimate of the absolute error
#  x[] - x-coordinates of data points
#  y[] - y-coordinates of data points
#  n   - number of data points
#  xi  - interpolation argument
#  err - estimate of the absolute error (output)
#----------------------------------------------------------------------------
   p = [[0]*(n+1) for i in range(n+1)]    # values of successive interpolants

   for i in range(1,n+1): p[i] = y[i]    # initialize with 0-order polynomial

   for m in range(1,n):                   # loop over columns of scheme array
      for i in range(1,n-m+1):                    # increase polynomial order
         p[i] = ((xi-x[m+i])*p[i] + (x[i]-xi)*p[i+1]) / (x[i]-x[m+i])

   yi = p[1]                                               # polynomial value
   err = fabs(p[1] - p[2])                                   # error estimate

   return (yi, err)

#============================================================================
def Spline(x, y, n, d1, dn, iopt, a, b, c, d, xi, yi, ni):
#----------------------------------------------------------------------------
#  Calculates the coefficients of the cubic splines for n data points and
#  returns interpolated values on a mesh of ni points
#  x[]    - x-coordinates of data points
#  y[]    - y-coordinates of data points
#  n      - number of data points
#  d1, dn - 1st derivatives at the endpoints x[1] and x[n]
#  iopt   - iopt == 0, natural splines: 2nd derivative = 0 at x[1] and x[n]
#           iopt == 1, clamped splines: uses the provided d1 and dn
#  a[], b[], c[], d[], i=1..n
#         - coefficients of cubic splines i=1..n-1 (output)
#  xi[]   - interpolation arguments
#  yi[]   - interpolant values (output)
#  ni     - number of interpolation points
#  Calls: TriDiagSys (linsys.py)
#----------------------------------------------------------------------------
   if (iopt == 0): d1 = dn = 0e0         # initialization for natural splines

   hi = 0e0; di = d1             # coefficients for system of 2nd derivatives
   for i in range(1,n):
      hm = hi; hi = x[i+1] - x[i]
      dm = di; di = (y[i+1] - y[i])/hi
      a[i] = hm; b[i] = 2e0*(hm + hi); c[i] = hi; d[i] = 6e0*(di - dm)

   a[n] = hi; b[n] = 2e0*hi; c[n] = 0e0; d[n] = 6e0*(dn - di)

   if (iopt == 0): c[1] = d[1] = a[n] = d[n] = 0e0          # natural splines

   TriDiagSys(a,b,c,d,n)                           # solve tridiagonal system
                                                       # 2nd derivatives in d
   for i in range(1,n):                                 # spline coefficients
      ip = i + 1
      xx = x[i]; xp = x[ip]; hi = xp - xx
      a[i] = (d[ip] - d[i]) / (6e0*hi)
      b[i] = (d[i]*xp - d[ip]*xx) / (2e0*hi)
      c[i] = (d[ip]*xx*xx - d[i]*xp*xp) / (2e0*hi) \
                                           + (y[ip] - y[i]) / hi - a[i]*hi*hi
      d[i] = (d[i]*xp*xp*xp - d[ip]*xx*xx*xx) / (6e0*hi) \
                                 + (y[i]*xp - y[ip]*xx) / hi - b[i]*hi*hi/3e0

   for i in range(1,ni):                                 # interpolation loop
      xx = xi[i]
      ip = 1
      while (ip < n-1 and xx > x[ip+1]): ip += 1           #  index of spline
      yi[i] = ((a[ip]*xx + b[ip])*xx + c[ip])*xx + d[ip]   #  evaluate spline

#============================================================================
def LinFit(x, y, sigmy, n, iopt):
#----------------------------------------------------------------------------
#  Determines the coefficients a and b of a linear model P(x;a,b) = a * x + b
#  and the associated uncertainties, sigma and sigmb, for a set of observed
#  data points by "Chi-square" regression
#  x[]     - x-coordinates of observed data, i = 1,..,n
#  y[]     - y-coordinates of observed data
#  sigmy[] - standard deviations of observed data
#  n       - number of observed data points
#  iopt    - iopt == 0 - initializes sigmy[i] = 1 (equal weights)
#          - iopt != 0 - uses the received values of sigmy[i]
#  a, b    - parameters of the linear model (output)
#  sigma   - uncertainties associated with the model parameters (output)
#  sigmb
#  chi2    - value of Chi-square merit function for the output parameters
#----------------------------------------------------------------------------
   if (iopt == 0):
      for i in range(1,n+1): sigmy[i] = 1e0        # iopt = 0: equall weights

   s = sx = sy = sxx = sxy = 0e0                               # prepare sums
   for i in range(1,n+1):
      f = 1e0/(sigmy[i]*sigmy[i])
      s  += f
      sx += x[i] * f; sxx += x[i] * x[i] * f
      sy += y[i] * f; sxy += x[i] * y[i] * f 

   f = 1e0/(s*sxx - sx*sx)
   a = (s *sxy - sx*sy ) * f; sigma = sqrt(s*f)           # model parameters
   b = (sy*sxx - sx*sxy) * f; sigmb = sqrt(sxx*f)         # and uncertainties

   chi2 = 0e0                                  # value of Chi-square function
   for i in range(1,n+1):
      f = (y[i] - a*x[i] - b)/sigmy[i]
      chi2 += f*f

   return (a, b, sigma, sigmb, chi2)

#============================================================================
def MultiFit(x, y, sigmy, n, iopt, a, sigma, npar, Func):
#----------------------------------------------------------------------------
#  Determines the coefficients a[i], i=1,..,npar of a multilinear model
#     F(x;a) = a[1] * func[1](x) + ... + a[npar] * func[npar](x)
#  and the associated uncertainties, sigma[i], for a set of observed data
#  points by "Chi-square" regression
#  x[]     - x-coordinates of observed data, i = 1,..,n
#  y[]     - y-coordinates of observed data
#  sigmy[] - standard deviations of observed data
#  n       - number of observed data points
#  iopt    - iopt == 0 - initializes sigmy[i] = 1 (equal weights)
#          - iopt != 0 - uses the received values of sigmy[i]
#  a[]     - parameters of the multilinear model (output)
#  sigma[] - uncertainties associated with the model parameters (output)
#  npar    - number of model parameters
#  chi2    - value of Chi-square merit function for the output parameters
#  Func()  - user function returning for a given argument x the values of the
#            basis functions func[i](x):
#               Func(x, func, npar)
#  Calls: GaussJordan (linsys.py)
#----------------------------------------------------------------------------
   c = [[0]*(n+1) for i in range(n+1)]
   b = [[0]*2 for i in range(n+1)]
   func = [0]*(npar+1)

   if (iopt == 0):
      for i in range(1,n+1): sigmy[i] = 1e0        # iopt = 0: equall weights

   for i in range(1,npar+1):                                 # initialization
      for j in range(1,npar+1): c[i][j] = 0e0
      b[i][1] = 0e0

   for i in range(1,n+1):                      # generate the system matrices
      yi = y[i]
      Func(x[i],func,npar)                     # evaluate the basis functions
      f = 1e0/(sigmy[i]*sigmy[i])
      for j in range(1,npar+1):
         fj = f * func[j]
         for k in range(1,npar+1): c[j][k] += fj * func[k]
         b[j][1] += fj * yi

   det = GaussJordan(c,b,npar,1)                           # solve the system

   for i in range(1,npar+1):
      a[i] = b[i][1]                                       # model parameters
      sigma[i] = sqrt(c[i][i])                      # parameter uncertainties

   chi2 = 0e0                                  # value of Chi-square function
   for i in range(1,n+1):
      Func(x[i],func,npar)                      # evaluate the model function
      f = 0e0
      for j in range(1,npar+1): f += a[j]*func[j]
      f = (y[i] - f)/sigmy[i]
      chi2 += f*f

   return chi2

#============================================================================
def PolFit(x, y, sigmy, n, iopt, a, sigma, npar):
#----------------------------------------------------------------------------
#  Determines the coefficients a[i], i=1,..,npar of a polynomial model
#     F(x;a) = a[1] * x^(npar-1) + a[2] * x^(npar-2) + ... + a[npar]
#  and the associated uncertainties, sigma[i], for a set of observed data
#  points by "Chi-square" regression
#  x[]     - x-coordinates of observed data, i = 1,..,n
#  y[]     - y-coordinates of observed data
#  sigmy[] - standard deviations of observed data
#  n       - number of observed data points
#  iopt    - iopt == 0 - initializes sigmy[i] = 1 (equal weights)
#          - iopt != 0 - uses the received values of sigmy[i]
#  a[]     - parameters of the polynomial model (output)
#  sigma[] - uncertainties associated with the model parameters (output)
#  npar    - number of model parameters (polynomial order + 1)
#  chi2    - value of Chi-square merit function for the output parameters
#  Calls: GaussJordan (linsys.py)
#----------------------------------------------------------------------------
   c = [[0]*(n+1) for i in range(n+1)]
   b = [[0]*2 for i in range(n+1)]
   func = [0]*(npar+1)

   if (iopt == 0):
      for i in range(1,n+1): sigmy[i] = 1e0        # iopt = 0: equall weights

   for i in range(1,npar+1):                                 # initialization
      for j in range(1,npar+1): c[i][j] = 0e0
      b[i][1] = 0e0

   for i in range(1,n+1):                      # generate the system matrices
      xi = x[i]; yi = y[i]
      func[npar] = 1e0                        # basis functions 1, x, x^2,...
      for j in range(npar-1,0,-1): func[j] = xi * func[j+1]
      f = 1e0/(sigmy[i]*sigmy[i])
      for j in range(1,npar+1):
         fj = f * func[j]
         for k in range(1,npar+1): c[j][k] += fj * func[k]
         b[j][1] += fj * yi

   det = GaussJordan(c,b,npar,1)                   # solution: corrections da

   for i in range(1,npar+1):
      a[i] = b[i][1]                                       # model parameters
      sigma[i] = sqrt(c[i][i])                      # parameter uncertainties

   chi2 = 0e0                                  # value of Chi-square function
   for i in range(1,n+1):
      xi = x[i]                                 # evaluate the model function
      f = a[1]
      for j in range(2,npar+1): f = f*xi + a[j]
      f = (y[i] - f)/sigmy[i]
      chi2 += f*f

   return chi2

#============================================================================
def Deriv(x, a, dFda, npar, Func):
#----------------------------------------------------------------------------
#  Evaluates the derivatives of model function Func(x,a,npar) at point x with
#  respect to parameters a[j], j=1,..,npar and returns them in dFda[j]
#----------------------------------------------------------------------------
   h0 = 1e-4                           # scale factor for the derivation step

   F0 = Func(x,a,npar)               # function value for original parameters
   for j in range(1,npar+1):
      h = h0 * fabs(a[j]) if (a[j]) else h0                 # derivation step
      temp = a[j]                                            # save parameter
      a[j] += h                                         # increment parameter
      dFda[j] = (Func(x,a,npar) - F0)/h                          # derivative
      a[j] = temp                                         # restore parameter

#============================================================================
def Chi2(x, y, sigmy, n, a, b, c, npar, Func):
#----------------------------------------------------------------------------
#  Evaluates the Chi2 merit function and the fitting matrices c and b for the
#  model function Func(x,a,npar) and the Levenberg-Marquardt algorithm
#  Calls: Deriv
#----------------------------------------------------------------------------
   dFda = [0]*(npar+1)                           # model function derivatives

   for k in range(1,npar+1):                                 # initialization
      for j in range(1,npar+1): c[k][j] = 0e0
      b[k] = 0e0

   chi2 = 0e0
   for i in range(1,n+1):
      dy = y[i] - Func(x[i],a,npar)            # deviation of model from data
      fsig = 1e0/(sigmy[i]*sigmy[i])
      chi2 += fsig * dy * dy

      Deriv(x[i],a,dFda,npar,Func)   # derivatives with respect to parameters
      for k in range(1,npar+1):                      # build fitting matrices
         fact = fsig * dFda[k]
         for j in range(1,npar+1): c[k][j] += fact * dFda[j]
         b[k] += fact * dy

   return chi2

#============================================================================
def MarqFit(x, y, sigmy, n, iopt, a, sigma, npar, Func):
#----------------------------------------------------------------------------
#  Minimizes the "Chi-square" merit function by the Levenberg-Marquardt
#  method, determining the coefficients a[i], i=1,..,npar of a non-linear
#  model F(x,a,npar) and the associated uncertainties, sigma[i], for a set of
#  observed data points
#  x[]     - x-coordinates of the observed data, i=1,..,n
#  y[]     - y-coordinates of the observed data
#  sigmy[] - standard deviations of the observed data
#  n       - number of observed data points
#  iopt    - iopt == 0 - initializes sigmy[i] = 1 (equal weights)
#          - iopt != 0 - uses the received values of sigmy[i]
#  a[]     - parameters of the non-linear model (output)
#  sigma[] - uncertainties of the model parameters (output)
#  npar    - number of model parameters
#  chi2    - value of Chi-square merit function for the output parameters
#  Func()  - user function returning for a given argument x the values of the
#            basis functions func[i](x):
#               Func(x, func, npar)
#  Calls: GaussJordan (linsys.py)
#----------------------------------------------------------------------------
   eps = 1e-4                                  # relative precision criterion
   itmax = 1000                                      # max. no. of iterations

   c   = [[0]*(n+1) for i in range(n+1)]
   c0  = [[0]*(n+1) for i in range(n+1)]
   cov = [[0]*(n+1) for i in range(n+1)]
   da  = [[0]*2 for i in range(n+1)]
   a1 = [0]*(n+1); b = [0]*(n+1); b0 = [0]*(n+1)
   func = [0]*(npar+1)

   if (iopt == 0):
      for i in range(1,n+1): sigmy[i] = 1e0        # iopt = 0: equall weights

   lam = 1e-3                                      # initial guess for lambda
   chi2 = Chi2(x,y,sigmy,n,a,b,c,npar,Func)

   it = 0
   while (it < itmax):                     # loop of parameter approximations
      chi20 = chi2
      for k in range(1,npar+1):            # store fitting matrices c0 and b0
         for j in range(1,npar+1): c0[k][j] = c[k][j]
         b0[k] = b[k]

      while (it < itmax):                           # loop of lambda increase
         it += 1
         for k in range(1,npar+1):                        # covariance matrix
            for j in range(1,npar+1): cov[k][j] = c0[k][j]
            cov[k][k] *= (1e0 + lam)                         # scale diagonal
            da[k][1] = b0[k]

         det = GaussJordan(cov,da,npar,1)          # solution: corrections da
         for j in range(1,npar+1): a1[j] = a[j] + da[j][1]     # trial params
         chi2 = Chi2(x,y,sigmy,n,a1,b,c,npar,Func)   # new linearized c and b
         if (chi2 <= chi20): break                   # stop increasing lambda
         lam *= 10e0                                        # increase lambda

      err = 0e0                        # maximum relative error of parameters
      for j in range(1,npar+1):
         a[j] += da[j][1]                                 # update parameters
         errj = fabs(da[j][1]/a[j]) if a[j] else fabs(da[j][1])
         if (err < errj): err = errj

      lam *= 0.1e0                                            # reduce lambda
                                                          # check convergence
      if ((err <= eps) and (fabs(1e0 - chi2/chi20) <= eps)): break

   if (it >= itmax): print("MarqFit: max. # of iterations exceeded !")
                                                # uncertainties of parameters
   for j in range(1,npar+1): sigma[j] = sqrt(cov[j][j])

   return chi2

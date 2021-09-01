#------------------------------- integral.py --------------------------------
#  Contains 1D and 3D integrators for real functions with real variables.
#  Part of the numxlib numerics library. Author: Titus Beu, 2013
#----------------------------------------------------------------------------
from math import *
from specfunc import *

#============================================================================
def qTrapz(Func, a, b, n):
#----------------------------------------------------------------------------
#  Integrates function Func on interval [a,b] using the trapezoidal rule
#  with n integration points
#----------------------------------------------------------------------------
   h = (b-a)/(n-1)
   s = 0.5*(Func(a) + Func(b))
   for i in range(1,n-1): s += Func(a+i*h)

   return h*s

#============================================================================
def qSimpson(Func, a, b, n):
#----------------------------------------------------------------------------
#  Integrates function Func on interval [a,b] using Simpson's rule with n
#  (odd) integration points
#----------------------------------------------------------------------------
   if (n % 2 == 0): n += 1                              # increment n if even

   h = (b-a)/(n-1)
   s1 = s2 = 0e0
   for i in range(2,n-2,2): s1 += Func(a + i*h)               # odd-index sum
   for i in range(1,n-1,2): s2 += Func(a + i*h)              # even-index sum

   return (h/3)*(Func(a) + 4*s2 + 2*s1 + Func(b))

#============================================================================
def qTrapzCtrl(Func, a, b, eps = 1e-6):
#----------------------------------------------------------------------------
#  Integrates function Func on interval [a,b] with relative precision eps
#  using the adaptive trapezoidal rule
#----------------------------------------------------------------------------
   kmax = 30                            # max. no. of step halving iterations

   h = b-a; n = 1
   t0 = 0.5*h*(Func(a) + Func(b))                     # initial approximation

   for k in range(1,kmax+1):                              # step halving loop
      sumf = 0e0
      for i in range(1,n+1): sumf += Func(a+(i-0.5)*h)
      t = 0.5*(t0 + h*sumf)                               # new approximation
      if (k > 1):                                         # convergence check
         if (fabs(t-t0) <= eps*fabs(t)): break
         if (fabs(t) <= eps and fabs(t) <= fabs(t-t0)): break # integral ~= 0
      h *= 0.5; n *= 2                               # halve integration step
      t0 = t

   if (k >= kmax): print("qTrapzCtrl: max. no. of iterations exceeded !")

   return t

#============================================================================
def qSimpsonCtrl(Func, a, b, eps = 1e-6):
#----------------------------------------------------------------------------
#  Integrates function Func on interval [a,b] with relative precision eps
#  using the adaptive Simpson rule
#----------------------------------------------------------------------------
   kmax = 30                            # max. no. of step halving iterations

   h = b-a; n = 1
   s0 = t0 = 0.5*h*(Func(a) + Func(b))                # initial approximation

   for k in range(1,kmax+1):                              # step halving loop
      sumf = 0e0
      for i in range(1,n+1): sumf += Func(a+(i-0.5)*h)
      t = 0.5*(t0 + h*sumf)
      s = (4*t - t0)/3                                    # new approximation
      if (k > 1):                                         # convergence check
         if (fabs(s-s0) <= eps*fabs(s)): break
         if (fabs(s) <= eps and fabs(s) <= fabs(s-s0)): break # integral ~= 0
      h *= 0.5; n *= 2                               # halve integration step
      s0 = s; t0 = t

   if (k >= kmax): print("qSimpsonCtrl: max. no. of iterations exceeded!")

   return s

#============================================================================
def qRomberg(Func, a, b, eps = 1e-6):
#----------------------------------------------------------------------------
#  Integrates function Func on interval [a,b] with relative precision eps
#  using the adaptive Romberg method
#----------------------------------------------------------------------------
   kmax = 30                            # max. no. of step halving iterations
   r1 = [0]*(kmax+1)                                  # two consecutive lines
   r2 = [0]*(kmax+1)                                  # from the method table

   h = b-a; n = 1
   r1[0] = 0.5*h*(Func(a) + Func(b))                  # initial approximation
   for k in range(1,kmax+1):                              # step halving loop
      sumf = 0e0
      for i in range(1,n+1): sumf += Func(a+(i-0.5)*h)
      r2[0] = 0.5*(r1[0] + h*sumf)                        # trapezoid formula
      f = 1e0
      for j in range(1,k+1):                      # increase quadrature order
         f *= 4
         r2[j] = (f*r2[j-1] - r1[j-1])/(f-1)              # new approximation

      if (k > 1):                                         # convergence check
         if (fabs(r2[k]-r1[k-1]) <= eps*fabs(r2[k])): break
         if (fabs(r2[k]) <= eps and fabs(r2[k]) <= fabs(r2[k]-r1[k-1])):break
      h *= 0.5; n *= 2                               # halve integration step
      for j in range(0,k+1): r1[j] = r2[j]                # shift table lines

   if (k >= kmax):
      print("qRomberg: max. no. of iterations exceeded !")
      k -= 1

   return r2[k]

#============================================================================
def qImprop1(Func, a, xinf, eps = 1e-6):
#----------------------------------------------------------------------------
#  Integrates function Func on interval [a,+inf) (for xinf >= 0) or (-inf,a]
#  (for xinf < 0) with relative precision eps. On output, xinf contains the
#  integration domain limit for which convergence was achieved.
#  Calls: qRomberg
#----------------------------------------------------------------------------
   h = 1e0; x = a    # subinterval length and initial left limit for [a,+inf)
   if (xinf < 0e0): h = -h; x = a + h                          # for (-inf,a]

   s1 = 1e0
   s = 0e0
   while(fabs(s1) > eps*fabs(s) or fabs(Func(x)) > eps):
      s1 = qRomberg(Func,x,x+fabs(h),eps)              # integral for [x,x+h]
      s += s1                                         # update total integral
      x += h                                         # shift interval [x,x+h]

   xinf = x                                       # final "infinite" boundary

   return (s, xinf)

#============================================================================
def qImprop2(Func, a, b, eps):
#----------------------------------------------------------------------------
#  Integrates function Func on interval [a,b] with a and/or b singular
#  integrable points.
#  Calls: qRomberg
#----------------------------------------------------------------------------
   h0 = 0.1e0 * (b-a)           # extent of vicinities of singular boundaries

   s = 0e0
   try: Func(a)
   except ZeroDivisionError:                            # a is singular point
      h = h0
      s1 = 1e0
      while(fabs(s1) > eps*fabs(s)): 
         h *= 0.5                                            # halve interval
         x = a + h                                 # left boundary of [x,x+h]
         s1 = qRomberg(Func,x,x+h,eps)          # partial integral on [x,x+h]
         s += s1                         # add contribution to total integral
      a += h0                            # new left boundary of core interval

   try: Func(b)
   except ZeroDivisionError:                            # b is singular point
      h = h0
      s1 = 1e0
      while(fabs(s1) > eps*fabs(s)):
         h *= 0.5                                            # halve interval
         x = b - h                                # right boundary of [x-h,x]
         s1 = qRomberg(Func,x-h,x,eps)          # partial integral on [x-h,x]
         s += s1                         # add contribution to total integral
      b -= h0                           # new right boundary of core interval

   s += qRomberg(Func,a,b,eps)                # add integral on core interval

   return s

#============================================================================
def qMidPoint(Func, a, b, eps = 1e-6):
#----------------------------------------------------------------------------
#  Integrates function Func on interval (a,b) with relative precision eps
#  using the adaptive midpoint rule
#----------------------------------------------------------------------------
   kmax = 19                                       # max. no. of subdivisions
   f1p6 = 1./6.; f5p6 = 5./6.

   h = b-a; n = 1
   s0 = h * Func(a+0.5*h)                             # initial approximation

   for k in range(1,kmax+1):                          # step subdivision loop
      sumf = 0e0
      for i in range(1,n+1): sumf += Func(a+(i-f5p6)*h) + Func(a+(i-f1p6)*h)
      s = (s0 + h*sumf)/3                                 # new approximation
      if (fabs(s - s0) <= eps*fabs(s)): break             # convergence check
      h /= 3; n *= 3                                            # reduce step
      s0 = s

   if (k >= kmax): print("qMidPoint: max. no. of iterations exceeded !")

   return s

#============================================================================
def xGaussLeg(a, b, n):
#----------------------------------------------------------------------------
#  Calculates abscissas x[] and weights w[] for the n-point Gauss-Legendre
#  quadrature on interval [a,b]
#  Calls: Legendre (from specfunc.py)
#----------------------------------------------------------------------------
   eps = 1e-14                                  # relative precision of zeros
   n2 = int(n/2)
   x = [0]*n
   w = [0]*n

   for i in range (0,n2):
      xi = cos(pi*(i+1-0.25e0)/(n+0.5e0))   # initial approximation for zeros
      f = 9e99
      while (fabs(f) > eps*fabs(xi)):             # Newton-Raphson refinement
         (f,d) = Legendre(n,xi); f /= d
         xi -= f
      x[i] = -xi; x[n-i-1] = xi                           # symmetrical zeros
      w[i] = w[n-i-1] = 2e0/((1e0-xi*xi)*d*d)                 # equal weights

   if (n % 2 == 1):                                  # odd no. of mesh points
      (f,d) = Legendre(n,0e0)
      x[n2] = 0e0
      w[n2] = 2e0/(d*d)

   f = 0.5e0*(b-a); xc = 0.5e0*(b+a)              # scaling to interval [a,b]
   for i in range (0,n):
      x[i] = f*x[i] + xc
      w[i] = f*w[i]

   return (x,w)

#============================================================================
def qGaussLeg(Func, a, b, n):
#----------------------------------------------------------------------------
#  Integrates function Func on interval [a,b] using n-point Gauss-Legendre
#  quadratures
#  Calls: xGaussLeg
#----------------------------------------------------------------------------
   x = [0]*n
   w = [0]*n

   (x,w) = xGaussLeg(a,b,n)

   s = 0e0
   for i in range(0,n): s += w[i] * Func(x[i])

   return s

#============================================================================
def xGaussLag(a, n):
#----------------------------------------------------------------------------
#  Calculates abscissas x[] and weights w[] for the n-point Gauss-Laguerre
#  quadrature on interval [a,+inf)
#  Calls: Laguerre (from specfunc.py)
#  Initial approximation for zeros:
#  A. Stroud & D. Secrest, Gaussian Quadrature Formulas, Prentice Hall, 1966.
#----------------------------------------------------------------------------
   eps = 1e-14                                  # relative precision of zeros
   x = [0]*n
   w = [0]*n

   for i in range(0,n):
      if (i == 0):       # initial approximation for zeros (Stroud & Secrest)
         xi = 3e0/(1e0+2.4e0*n)                                    # 1st zero
      elif (i == 1):
         xi = 15e0/(1e0+2.5e0*n) + x[0]                            # 2nd zero
      else:
         f = (1e0/(i+1)+2.55e0)/1.9e0
         xi = (1e0+f)*x[i-1] - f*x[i-2]                          # recurrence

      f = 9e99
      while (fabs(f) > eps*fabs(xi)):             # Newton-Raphson refinement
         (f,d) = Laguerre(n,xi); f /= d
         xi -= f
      x[i] = xi
      w[i] = exp(xi)/(xi*d*d)
             
   for i in range(0,n): x[i] += a              # scaling to interval [a,+inf)

   return (x,w)

#============================================================================
def qGaussLag(Func, a, n):
#----------------------------------------------------------------------------
#  Integrates function Func on interval [a,+inf) using n-point Gauss-Laguerre
#  quadratures
#  Calls: xGaussLag
#----------------------------------------------------------------------------
   x = [0]*n
   w = [0]*n

   (x,w) = xGaussLag(a,n)

   s = 0e0
   for i in range(0,n): s += w[i] * Func(x[i])

   return s

#============================================================================
def qTrapz3D(Func, ax, bx, nx, ay, by, ny, az, bz, nz):
#----------------------------------------------------------------------------
#  Integrates function Func(x,y,z) in the cuboid [ax,bx] x [ay,by] x [az,bz]
#  using the trapezoidal rule with (nx x ny x nz) integration points
#----------------------------------------------------------------------------
   hx = (bx-ax)/(nx-1)
   hy = (by-ay)/(ny-1)
   hz = (bz-az)/(nz-1)

   s = 0e0
   for i in range(0,nx):
      x = ax + i*hx; wx = (hx if i*(i+1-nx) else 0.5e0*hx)
      sx = 0e0
      for j in range(0,ny):
         y = ay + j*hy; wy = (hy if j*(j+1-ny) else 0.5e0*hy)
         sy = 0e0
         for k in range(0,nz):
            z = az + k*hz; wz = (hz if k*(k+1-nz) else 0.5e0*hz)
            sy += wz * Func(x,y,z)
         sx += wy * sy
      s += wx * sx

   return s

#============================================================================
def xSimpson(a, b, n):
#----------------------------------------------------------------------------
#  Calculates abscissas x[] and weights w[] for Simpson's rule with n
#  integration points on interval [a,b]
#----------------------------------------------------------------------------
   c13 = 1e0/3e0; c23 = 2e0/3e0; c43 = 4e0/3e0

   if (n % 2 == 0): n += 1                              # increment n if even

   x = [0]*n; w = [0]*n

   h = (b-a)/(n-1)
   for i in range(0,n):
      x[i] = a + i*h; w[i] = h * (c23 if (i+1) % 2 else c43)
   w[0] = w[n-1] = h * c13

   return (x, w)

#============================================================================
def qSimpson3D(Func, ax, bx, nx, ay, by, ny, az, bz, nz):
#----------------------------------------------------------------------------
#  Integrates function Func(x,y,z) in the cuboid [ax,bx] x [ay,by] x [az,bz]
#  using Simpson's rule with (nx x ny x nz) integration points
#----------------------------------------------------------------------------
   if (nx % 2 == 0): nx += 1                           # increment nx if even
   if (ny % 2 == 0): ny += 1                           # increment ny if even
   if (nz % 2 == 0): nz += 1                           # increment nz if even

   x = [0]*nx; wx = [0]*nx
   y = [0]*ny; wy = [0]*ny
   z = [0]*nz; wz = [0]*nz
   
   (x,wx) = xSimpson(ax,bx,nx)                  # generate integartion points
   (y,wy) = xSimpson(ay,by,ny)
   (z,wz) = xSimpson(az,bz,nz)

   s = 0e0
   for i in range(0,nx):
      sx = 0e0
      for j in range(0,ny):
         sy = 0e0
         for k in range(0,nz):
            sy += wz[k] * Func(x[i],y[j],z[k])
         sx += wy[j] * sy
      s += wx[i] * sx

   return s

#============================================================================
def qGaussLeg3D(Func, ax, bx, nx, ay, by, ny, az, bz, nz):
#----------------------------------------------------------------------------
#  Integrates function Func(x,y,z) in the cuboid [ax,bx] x [ay,by] x [az,bz]
#  using Gauss-Legendre quadratures with (nx x ny x nz) integration points
#----------------------------------------------------------------------------
   x = [0]*nx; wx = [0]*nx
   y = [0]*ny; wy = [0]*ny
   z = [0]*nz; wz = [0]*nz

   (x,wx) = xGaussLeg(ax,bx,nx)                 # generate integartion points
   (y,wy) = xGaussLeg(ay,by,ny)
   (z,wz) = xGaussLeg(az,bz,nz)

   s = 0e0
   for i in range(0,nx):
      sx = 0e0
      for j in range(0,ny):
         sy = 0e0
         for k in range(0,nz):
            sy += wz[k] * Func(x[i],y[j],z[k])
         sx += wy[j] * sy
      s += wx[i] * sx

   return s

#============================================================================
def qSimpsonAng(Func, nt, np):
#----------------------------------------------------------------------------
#  Integrates function Func(theta,phi) on [0,pi] x [0,2*pi] in sperical
#  coordinates using Simpson's rule with (nt x np) points
#----------------------------------------------------------------------------
   if (nt % 2 == 0): nt += 1                           # increment nt if even
   if (np % 2 == 0): np += 1                           # increment np if even

   t = [0]*nt; wt = [0]*nt
   p = [0]*np; wp = [0]*np

   (t,wt) = xSimpson(0e0,pi,nt)
   (p,wp) = xSimpson(0e0,2e0*pi,np)

   for i in range(0,nt): wt[i] *= sin(t[i])                  # volume element

   s = 0e0
   for i in range(0,nt):
      st = 0e0
      for j in range(0,np):
         st += wp[j] * Func(t[i],p[j])
      s += wt[i] * st

   return s

#============================================================================
def qSimpsonSph(Func, a, nr, nt, np):
#----------------------------------------------------------------------------
#  Integrates function Func(r,theta,phi) on [0,a] x [0,pi] x [0,2*pi]
#  in sperical coordinates using Simpson's rule with (nr x nt x np) points
#----------------------------------------------------------------------------
   if (nr % 2 == 0): nr += 1                           # increment nr if even
   if (nt % 2 == 0): nt += 1                           # increment nt if even
   if (np % 2 == 0): np += 1                           # increment np if even

   r = [0]*nr; wr = [0]*nr
   t = [0]*nt; wt = [0]*nt
   p = [0]*np; wp = [0]*np

   (r,wr) = xSimpson(0e0,a,nr)                  # generate integartion points
   (t,wt) = xSimpson(0e0,pi,nt)
   (p,wp) = xSimpson(0e0,2e0*pi,np)

   for i in range(0,nr): wr[i] *= r[i] * r[i]                # factors from
   for j in range(0,nt): wt[j] *= sin(t[j])                  # volume element

   s = 0e0
   for i in range(0,nr):
      sr = 0e0
      for j in range(0,nt):
         st = 0e0
         for k in range(0,np):
            st += wp[k] * Func(r[i],t[j],p[k])
         sr += wt[j] * st
      s += wr[i] * sr

   return s

#============================================================================
def qGaussSph(Func, nr, nt, np):
#----------------------------------------------------------------------------
#  Integrates function Func(r,theta,phi) on [0,inf] x [0,pi] x [0,2*pi]
#  in spherical coordinates using Gauss-Laguerre and Gauss-Legendre formulas
#  with (nr x nt x np) points
#----------------------------------------------------------------------------
   r = [0]*nr; wr = [0]*nr
   t = [0]*nt; wt = [0]*nt
   p = [0]*np; wp = [0]*np

   (r,wr) = xGaussLag(0e0,nr)              # Gauss-Laguerre radial quadrature
   (t,wt) = xGaussLeg(0e0,pi,nt)         # Gauss-Legendre angular quadratures
   (p,wp) = xGaussLeg(0e0,2e0*pi,np)

   for i in range(0,nr): wr[i] *= r[i] * r[i]                # factors from
   for j in range(0,nt): wt[j] *= sin(t[j])                  # volume element

   s = 0e0
   for i in range(0,nr):
      sr = 0e0
      for j in range(0,nt):
         st = 0e0
         for k in range(0,np):
            st += wp[k] * Func(r[i],t[j],p[k])
         sr += wt[j] * st
      s += wr[i] * sr

   return s

#============================================================================
def qSimpsonCyl(Func, a, az, bz, nr, np, nz):
#----------------------------------------------------------------------------
#  Integrates function Func(r,phi,z) on domain [0,a] x [0,2*pi] x [az,bz]
#  in cylindrical coordinates using Simpson's rule with (nr x np x nz) points
#----------------------------------------------------------------------------
   if (nr % 2 == 0): nr += 1                           # increment nr if even
   if (np % 2 == 0): np += 1                           # increment np if even
   if (nz % 2 == 0): nz += 1                           # increment nz if even

   r = [0]*nr; wr = [0]*nr
   p = [0]*np; wp = [0]*np
   z = [0]*nz; wz = [0]*nz

   (r,wr) = xSimpson(0e0,a,nr)                  # generate integartion points
   (p,wp) = xSimpson(0e0,2e0*pi,np)
   (z,wz) = xSimpson(az,bz,nz)

   for i in range(0,nr): wr[i] *= r[i]           # factor from volume element

   s = 0e0
   for i in range(0,nr):
      sr = 0e0
      for j in range(0,np):
         sp = 0e0
         for k in range(0,nz):
            sp += wz[k] * Func(r[i],p[j],z[k])
         sr += wp[j] * sp
      s += wr[i] * sr

   return s

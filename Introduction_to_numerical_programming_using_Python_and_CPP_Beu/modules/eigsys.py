#-------------------------------- eigsys.py ---------------------------------
#  Contains routines for solving eigenvalue problems.
#  Part of the numxlib numerics library. Author: Titus Beu, 2013
#----------------------------------------------------------------------------
from linsys import *

#============================================================================
def Jacobi(a, x, d, n):
#----------------------------------------------------------------------------
#  Solves the eigenvalue problem of a real symmetric matrix using the
#  Jacobi method
#  a  - real symmetric matrix (lower triangle is destroyed)
#  x  - modal matrix: eigenvectors on columns (output)
#  d  - vector of eigenvalues
#  n  - order of matrix a
#  Error flag: 0 - normal execution, 1 - exceeded max. no. of iterations
#----------------------------------------------------------------------------
   eps = 1e-30                                          # precision criterion
   itmax = 50                                         # max no. of iterations

   for i in range(1,n+1):                                    # initialization
      for j in range(1,n+1): x[i][j] = 0e0       # modal matrix = unit matrix
      x[i][i] = 1e0
      d[i] = a[i][i]                        # eigenvalues = diagonal elements

   for it in range(1,itmax+1):                           # loop of iterations
      amax = 0e0
      for i in range(2,n+1):                          # lower triangle: i > j
         for j in range(1,i):
            aii = d[i]; ajj = d[j]                        # diagonal elements
            aij = fabs(a[i][j])
            if (aij > amax): amax = aij           # max. non-diagonal element
            if (aij > eps):                                # perform rotation
               c = 0.5e0*(aii-ajj)/a[i][j]
               t = 1e0/(fabs(c) + sqrt(1e0+c*c))                    # tangent
               if (c < 0e0): t = -t                                    # sign
               c = 1e0/sqrt(1e0+t*t); s = c*t                      # cos, sin
               for k in range(1,j):                           # columns k < j
                  t       = a[j][k]*c - a[i][k]*s
                  a[i][k] = a[i][k]*c + a[j][k]*s
                  a[j][k] = t
               for k in range(j+1,i):                         # columns k > j
                  t       = a[k][j]*c - a[i][k]*s        # interchange j <> k
                  a[i][k] = a[i][k]*c + a[k][j]*s
                  a[k][j] = t
               for k in range(i+1,n+1):                       # columns k > i
                  t       = a[k][j]*c - a[k][i]*s        # interchange i <> k
                  a[k][i] = a[k][i]*c + a[k][j]*s        # interchange j <> k
                  a[k][j] = t

               for k in range(1,n+1):                # transform modal matrix
                  t       = x[k][j]*c - x[k][i]*s
                  x[k][i] = x[k][i]*c + x[k][j]*s
                  x[k][j] = t

               t = 2e0 * s * c * a[i][j]
               d[i] = aii*c*c + ajj*s*s + t              # update eigenvalues
               d[j] = ajj*c*c + aii*s*s - t
               a[i][j] = 0e0

      if (amax<=eps): break                               # check convergence

   if (it > itmax):
      print("Jacobi: max. no. of iterations exceeded !"); return 1
   return 0

#============================================================================
def EigSym(a, b, x, d, n):
#----------------------------------------------------------------------------
#  Solves the generalized eigenvalue problem a x = lambda b x for the real
#  symmetric matrix a and the real positive-definite symmetric matrix b,
#  reducing it to standard form at xt = lambda xt by Cholesky factorization
#  of matrix b = L * LT.
#  a  - real symmetric matrix
#  b  - real positive-definite symmetric matrix (L replaces lower triangle)
#  x  - modal matrix: eigenvectors on columns (output)
#  d  - vector of eigenvalues
#  n  - order of matrix a
#  Error flag: 0 - normal execution
#              1 - matrix b not positive-definite
#  Calls: Cholesky, MatTriInv, Jacobi.
#----------------------------------------------------------------------------
   (det,ierr) = Cholesky(b,n)             # Cholesky factorization b = L * LT
   if (ierr):
      print("EigSym: matrix b is mot positive-definite !"); return 1

   MatTriInv(b,n)                                                # b = L^(-1)

   for i in range(1,n):                            # fill upper triangle of a
      for j in range(i+1,n+1): a[i][j] = a[j][i]

   at = [[0]*(n+1) for i in range(n+1)]
   xt = [[0]*(n+1) for i in range(n+1)]

   for i in range(1,n+1):      # transformed matrix at = L^(-1) * a * LT^(-1)
      for j in range(1,i+1):
         sum = 0e0
         for k in range(1,i+1):
            for m in range(1,j+1): sum += b[i][k] * a[k][m] * b[j][m]
         at[i][j] = sum

   Jacobi(at,xt,d,n)            # solve transformed problem at xt = lambda xt

   for j in range(1,n+1):                         # recompose eigenvectors of
      for i in range(1,n+1):                     # initial eigenvalue problem
         sum = 0e0
         for k in range(i,n+1): sum += b[k][i] * xt[k][j]
         x[i][j] = sum

   return 0

#============================================================================
def EigSort(x, d, n, isort):
#----------------------------------------------------------------------------
#  Sorts the eigenvalues d and eigenvectors x according to the eigenvalues
#  x  - eigenvectors on columns
#  d  - vector of eigenvalues
#  n  - dimension of eigenvalue problem
#  isort = 0: no sorting; > 0: ascending sorting; < 0: descending sorting
#----------------------------------------------------------------------------
   if (isort == 0): return

   for j in range(1,n):
      jmax = j; dmax = d[j]               # find column of maximum eigenvalue
      for i in range(j+1,n+1):
         if (isort * (dmax - d[i]) > 0e0):
            jmax = i; dmax = d[i]

      if (jmax != j):
         d[jmax] = d[j]; d[j] = dmax    # swap current component with maximum
         for i in range(1,n+1):
            t = x[i][j]; x[i][j] = x[i][jmax]; x[i][jmax] = t

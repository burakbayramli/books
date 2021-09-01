#--------------------------------- linsys.py --------------------------------
#  Contains routines for solving systems of linear equations.
#  Part of the numxlib numerics library. Author: Titus Beu, 2013
#----------------------------------------------------------------------------
from math import *

#============================================================================
def Gauss(a, b, n, m):
#----------------------------------------------------------------------------
#  Solves matrix equation a x = b by Gaussian elimination with partial
#  pivoting on columns.
#  a   - coefficient matrix (n x n); destroyed on exit
#  b   - matrix of constant terms (n x m); solution x on exit
#  det - determinant of coefficient matrix (output).
#----------------------------------------------------------------------------
   det = 1e0
   for k in range(1,n+1):                               # FORWARD ELIMINATION
      amax = 0e0                                   # determine pivot row with
      for i in range(k,n+1):                       # max. element on column k
         if (amax < fabs(a[i][k])): amax = fabs(a[i][k]); imax = i
      if (amax == 0e0):
         print("Gauss: singular matrix !"); return 0e0

      if (imax != k):                           # interchange rows imax and k
         det = -det                             # to put pivot on diagonal
         for j in range(k,n+1): (a[imax][j],a[k][j]) = (a[k][j],a[imax][j])
         for j in range(1,m+1): (b[imax][j],b[k][j]) = (b[k][j],b[imax][j])

      det *= a[k][k]                        # multiply determinant with pivot

      t = 1e0/a[k][k]                             # divide pivot row by pivot
      for j in range(k+1,n+1): a[k][j] *= t
      for j in range(  1,m+1): b[k][j] *= t

      for i in range(k+1,n+1):                        # reduce non-pivot rows
         t = a[i][k]
         for j in range(k+1,n+1): a[i][j] -= a[k][j]*t
         for j in range(  1,m+1): b[i][j] -= b[k][j]*t

   for k in range(n-1,0,-1):                          # BACKWARD SUBSTITUTION
      for j in range(1,m+1):
         sum = b[k][j]
         for i in range(k+1,n+1): sum -= a[k][i]*b[i][j]
         b[k][j] = sum

   return det

#============================================================================
def GaussJordan0(a, b, n, m):
#----------------------------------------------------------------------------
#  Solves matrix equation a x = b by Gauss-Jordan elimination with partial
#  pivoting on columns.
#  a   - coefficient matrix (n x n); destroyed on exit
#  b   - matrix of constant terms (n x m); solution x on exit
#  det - determinant of coefficient matrix (output).
#----------------------------------------------------------------------------
   det = 1e0
   for k in range(1,n+1):                               # FORWARD ELIMINATION
      amax = 0e0                                   # determine pivot row with
      for i in range(k,n+1):                       # max. element on column k
         if (amax < fabs(a[i][k])): amax = fabs(a[i][k]); imax = i
      if (amax == 0e0):
         print("GaussJordan0: singular matrix !"); return 0e0

      if (imax != k):                           # interchange rows imax and k
         det = -det                             # to put pivot on diagonal
         for j in range(k,n+1): (a[imax][j],a[k][j]) = (a[k][j],a[imax][j])
         for j in range(1,m+1): (b[imax][j],b[k][j]) = (b[k][j],b[imax][j])

      det *= a[k][k]                        # multiply determinant with pivot

      t = 1e0/a[k][k]                             # divide pivot row by pivot
      for j in range(k+1,n+1): a[k][j] *= t
      for j in range(  1,m+1): b[k][j] *= t

      for i in range(1,n+1):                          # reduce non-pivot rows
         if (i != k): 
            t = a[i][k]
            for j in range(1,n+1): a[i][j] -= a[k][j]*t
            for j in range(1,m+1): b[i][j] -= b[k][j]*t

   return det

#============================================================================
def GaussJordan1(a, b, n, m):
#----------------------------------------------------------------------------
#  Solves matrix equation a x = b by Gauss-Jordan elimination with partial
#  pivoting on columns.
#  a   - coefficient matrix (n x n); a^(-1) on exit
#  b   - matrix of constant terms (n x m); solution x on exit
#  det - determinant of coefficient matrix (output).
#----------------------------------------------------------------------------
   ipivot = [0]*(n+1)                                     # stores pivot rows

   det = 1e0
   for k in range(1,n+1):                               # FORWARD ELIMINATION
      amax = 0e0                                   # determine pivot row with
      for i in range(k,n+1):                       # max. element on column k
         if (amax < fabs(a[i][k])): amax = fabs(a[i][k]); imax = i
      if (amax == 0e0):
         print("GaussJordan1: singular matrix !"); return 0e0
      ipivot[k] = imax                                # store pivot row index

      if (imax != k):                           # interchange rows imax and k
         det = -det                             # to put pivot on diagonal
         for j in range(1,n+1): (a[imax][j],a[k][j]) = (a[k][j],a[imax][j])
         for j in range(1,m+1): (b[imax][j],b[k][j]) = (b[k][j],b[imax][j])

      det *= a[k][k]                        # multiply determinant with pivot

      t = 1e0/a[k][k]                             # divide pivot row by pivot
      a[k][k] = 1e0                         # diagonal element of unit matrix
      for j in range(1,n+1): a[k][j] *= t
      for j in range(1,m+1): b[k][j] *= t

      for i in range(1,n+1):                          # reduce non-pivot rows
         if (i != k): 
            t = a[i][k]
            a[i][k] = 0e0               # non-diagonal element of unit matrix
            for j in range(1,n+1): a[i][j] -= a[k][j]*t
            for j in range(1,m+1): b[i][j] -= b[k][j]*t

   for k in range(n,0,-1):                     # rearrange columns of inverse
      imax = ipivot[k]
      if (imax != k):
         for i in range(1,n+1): (a[i][imax],a[i][k]) = (a[i][k],a[i][imax])

   return det

#============================================================================
def GaussJordan(a, b, n, m):
#----------------------------------------------------------------------------
#  Solves matrix equation a x = b by Gauss-Jordan elimination with complete
#  pivoting.
#  a   - coefficient matrix (n x n); a^(-1) on exit
#  b   - matrix of constant terms (n x m); solution x on exit
#  det - determinant of coefficient matrix (output).
#----------------------------------------------------------------------------
   ipivot = [0]*(n+1); jpivot = [0]*(n+1)     # stores pivot rows and columns
   npivot = [0]*(n+1)                              # marks used pivot columns

   det = 1e0
   for k in range(1,n+1):                               # FORWARD ELIMINATION
      amax = 0e0                                            # determine pivot
      for i in range(1,n+1):                                 # loop over rows
         if (npivot[i] != 1):
            for j in range(1,n+1):                        # loop over columns
               if (npivot[j] == 0):                  # pivoting not yet done?
                  if (amax < fabs(a[i][j])):
                     amax = fabs(a[i][j]); imax = i; jmax = j
               if (npivot[j] > 1):
                  print("GaussJordan: singular matrix 1 !"); return 0e0
      if (amax == 0e0): print("GaussJordan: singular matrix 2 !"); return 0e0

      ipivot[k] = imax; jpivot[k] = jmax         # store pivot row and column
      npivot[jmax] = npivot[jmax] + 1                # mark used pivot column

      if (imax != jmax):                     # interchange rows imax and jmax
         det = -det                          # to put pivot on diagonal
         for j in range(1,n+1):
            (a[imax][j],a[jmax][j]) = (a[jmax][j],a[imax][j])
         for j in range(1,m+1):
            (b[imax][j],b[jmax][j]) = (b[jmax][j],b[imax][j])

      det *= a[jmax][jmax]                  # multiply determinant with pivot

      t = 1e0/a[jmax][jmax]                       # divide pivot row by pivot
      a[jmax][jmax] = 1e0                   # diagonal element of unit matrix
      for j in range(1,n+1): a[jmax][j] *= t
      for j in range(1,m+1): b[jmax][j] *= t

      for i in range(1,n+1):                          # reduce non-pivot rows
         if (i != jmax): 
            t = a[i][jmax]
            a[i][jmax] = 0e0            # non-diagonal element of unit matrix
            for j in range(1,n+1): a[i][j] -= a[jmax][j]*t
            for j in range(1,m+1): b[i][j] -= b[jmax][j]*t

   for k in range(n,0,-1):                     # rearrange columns of inverse
      imax = ipivot[k]; jmax = jpivot[k]
      if (imax != jmax):
         for i in range(1,n+1):
            (a[i][imax],a[i][jmax]) = (a[i][jmax],a[i][imax])

   return det

#============================================================================
def LUFactor(a, ipivot, n):
#----------------------------------------------------------------------------
#  Performs LU factorization of (n x n) matrix a (diag(L) = 1). On exit,
#  replaces upper triangle and diagonal with U, and lower triangle, with L.
#  Uses partial pivoting on columns.
#  a      - coefficient matrix (n x n); LU decomposition on exit
#  ipivot - array of pivot row indexes (output)
#  det    - determinant of coefficient matrix (output).
#----------------------------------------------------------------------------
   det = 1e0
   for j in range(1,n+1):                                 # loop over columns
      for i in range(1,j):                             # elements of matrix U
         sum = a[i][j]
         for k in range(1,i): sum -= a[i][k]*a[k][j]
         a[i][j] = sum

      amax = 0e0
      for i in range(j,n+1):                           # elements of matrix L
         sum = a[i][j]                                 # undivided by pivot
         for k in range(1,j): sum -= a[i][k]*a[k][j]
         a[i][j] = sum
                                                            # determine pivot
         if (amax < fabs(a[i][j])): amax = fabs(a[i][j]); imax = i

      if (amax == 0e0): print("LUFactor: singular matrix !"); return 0e0

      ipivot[j] = imax                                # store pivot row index
                                                # interchange rows imax and j
      if (imax != j):                           # to put pivot on diagonal
         det = -det
         for k in range(1,n+1):
            t = a[imax][k]; a[imax][k] = a[j][k]; a[j][k] = t

      det *= a[j][j]                        # multiply determinant with pivot

      t = 1e0/a[j][j]                         # divide elements of L by pivot
      for i in range(j+1,n+1): a[i][j] *= t

   return det

#============================================================================
def LUSystem(a, ipivot, b, n):
#----------------------------------------------------------------------------
#  Solves linear system a x = b of order n by LU factorization.
#  a      - LU decomposition of coefficient matrix (returned by LUFactor)
#  ipivot - array of pivot row indexes (input)
#  b      - vector of constant terms (input); solution x (on exit)
#----------------------------------------------------------------------------
   for i in range(1,n+1):                                     # solves Ly = b
      sum = b[ipivot[i]]
      b[ipivot[i]] = b[i]
      for j in range(1,i): sum -= a[i][j]*b[j]
      b[i] = sum

   for i in range(n,0,-1):                                    # solves Ux = y
      sum = b[i]
      for j in range(i+1,n+1): sum -= a[i][j]*b[j]
      b[i] = sum/a[i][i]

#============================================================================
def MatInv(a, n):
#----------------------------------------------------------------------------
#  Calculates inverse of (n x n) matrix a by LU factorization.
#  a   - (n x n) matrix (input); a^(-1) (output)
#  det - determinant of coefficient matrix (output).
#  Calls: LUFactor, LUSystem.
#----------------------------------------------------------------------------
   ainv = [[0]*(n+1) for i in range(n+1)]     # temporary storage for inverse
   b = [0]*(n+1)
   ipivot = [0]*(n+1)                                     # stores pivot rows

   det = LUFactor(a,ipivot,n)                         # LU factorization of a
   if (det == 0e0):                                         # singular matrix
      print("MatInv: singular matrix !"); return 0e0

   for j in range(1,n+1):                  # loop over columns of unit matrix
      for i in range(1,n+1): b[i] = 0e0                            # column j
      b[j] = 1e0                          
      LUSystem(a,ipivot,b,n)                                   # solve system
      for i in range(1,n+1): ainv[i][j] = b[i]          # column j of inverse

   for j in range(1,n+1):                                 # copy inverse in a
      for i in range(1,n+1): a[i][j] = ainv[i][j]

   return det

#============================================================================
def MatTriInv(a, n):
#----------------------------------------------------------------------------
#  Calculates inverse of lower triangular matrix L of order n contained in
#  (n x n) array a and replaces on exit lower triangle of a by L^(-1).
#  Error flag = 0 - normal execution
#               1 - singular matrix.
#----------------------------------------------------------------------------
   for j in range(1,n+1):                                 # loop over columns
      if (a[j][j] <= 0e0): print("MatTriInv: singular matrix !"); return 1
      a[j][j] = 1e0/a[j][j]                                # diagonal element

      for i in range(j+1,n+1):                        # non-diagonal elements
         sum = 0e0
         for k in range(j,i): sum -= a[i][k]*a[k][j]
         a[i][j] = sum/a[i][i]

   return 0

#============================================================================
def Cholesky(a, n):
#----------------------------------------------------------------------------
#  Performs Cholesky factorization L * LT of real symmetric positive-definite
#  (n x n) matrix a. On exit, replaces lower triangle of a with L.
#  a   - coefficient matrix (n x n); L in lower triangle and diagonal on exit
#  det - determinant of coefficient matrix (output).
#  Error flag = 0 - normal execution
#               1 - a is not positive-definite.
#----------------------------------------------------------------------------
   det = 1e0
   for j in range(1,n+1):                                 # loop over columns
      sum = a[j][j]                                        # diagonal element
      for k in range(1,j): sum -= a[j][k]*a[j][k]
      if (sum <= 0e0): 
         print("Cholesky: matrix b is not positive-definite !"); return (0,1)
      a[j][j] = sqrt(sum)

      for i in range(j+1,n+1):                         # non-diagonal elements
         sum = a[i][j]
         for k in range(1,j): sum -= a[i][k]*a[j][k]
         a[i][j] = sum / a[j][j]

      det *= a[j][j]*a[j][j]

   return (det,0)

#============================================================================
def CholeskySys(a, b, n):
#----------------------------------------------------------------------------
#  Solves linear system a x = b with real symmetric positive-definite (n x n)
#  matrix by Cholesky factorization L * LT.
#  a - matrix L in lower triangle and on diagonal (returned by Cholesky)
#  b - vector of constant terms (input); solution x (on exit)
#----------------------------------------------------------------------------
   for i in range(1,n+1):                                     # solves Ly = b
      sum = b[i]
      for j in range(1,i): sum -= a[i][j]*b[j]
      b[i] = sum/a[i][i]

   for i in range(n,0,-1):                                  # solves LT x = y
      sum = b[i]
      for j in range(i+1,n+1): sum -= a[j][i]*b[j]
      b[i] = sum/a[i][i]

#============================================================================
def MatSymInv(a, n):
#----------------------------------------------------------------------------
#  Calculates inverse of symmetric positive-definite (n x n) matrix by
#  Cholesky factorization a = L * LT as a^(-1) = (L^(-1))T * L^(-1).
#  L^(-1) is calculated using MatTriInv.
#  a   - (n x n) matrix (input); a^(-1) (output)
#  det - determinant of coefficient matrix (output).
#  Calls: Cholesky, MatTriInv.
#----------------------------------------------------------------------------
   (det,ierr) = Cholesky(a,n)                     # L * LT factorization of a
   if (ierr == 1):
      print("MatSymInv: matrix not positive-definite !"); return det
   if (det == 0e0): print("MatSymInv: singular matrix !"); return det

   MatTriInv(a,n)                                  # L^(-1) in lower triangle

   for i in range(1,n+1):                                # (L^(-1))T * L^(-1)
      for j in range(1,i+1):
         sum = 0e0
         for k in range(i,n+1): sum += a[k][i] * a[k][j]
         a[j][i] = sum                              # store in upper triangle

   for i in range(1,n+1):                           # complete lower triangle
      for j in range(1,i+1): a[i][j] = a[j][i]

   return det

#============================================================================
def TriDiagSys(a, b, c, d, n):
#----------------------------------------------------------------------------
#  Solves a system with tridiagonal matrix by LU factorization (diag(L) = 1).
#  a - lower codiagonal (i=2,n)
#  b - main diagonal (i=1,n)
#  c - upper codiagonal (i=1,n-1)
#  d - constant terms (i=1,n); solution on exit
#  n - order of system.
#----------------------------------------------------------------------------
   if (b[1] == 0e0): print("TriDiagSys: singular matrix !"); return
   for i in range(2,n+1):                                     # factorization
      a[i] /= b[i-1]
      b[i] -= a[i]*c[i-1]
      if (b[i] == 0e0): print("TriDiagSys: singular matrix !"); return
      d[i] -= a[i]*d[i-1]

   d[n] /= b[n]                                       # backward substitution
   for i in range(n-1,0,-1): d[i] = (d[i] - c[i]*d[i+1])/b[i]

#============================================================================
def GaussSeidel(a, b, x, n, init):
#----------------------------------------------------------------------------
#  Solves linear system a x = b by the Gauss-Seidel method.
#  Convergence is ensure by left-multiplying the system with a^T.
#  a    - coefficient matrix (n x n)
#  b    - vector of constant terms
#  x    - initial approximation of solution (input); solution (output)
#  n    - order of system
#  err  - maximum relative error of the solution components
#  init - initialization option: 0 - refines initial approximation 
#                                1 - initializes solution
#----------------------------------------------------------------------------
   eps = 1e-10                                 # relative precision criterion
   itmax = 1000                                       # max no. of iterations

   s = [[0]*(n+1) for i in range(n+1)]           # matrices of reduced system
   t = [0]*(n+1)

   for i in range(1,n+1):                         # matrices of normal system
      for j in range(1,i+1):                      # by multiplication with aT
         s[i][j] = 0e0                            # store result in s and t
         for k in range(1,n+1): s[i][j] += a[k][i]*a[k][j]
         s[j][i] = s[i][j]

      t[i] = 0e0
      for j in range(1,n+1): t[i] += a[j][i]*b[j]

   for i in range(1,n+1):                # matrices s and t of reduced system
      f = -1e0/s[i][i]; t[i] /= s[i][i]
      for j in range(1,n+1): s[i][j] *= f

   if (init):
      for i in range(1,n+1): x[i] = t[i]                # initialize solution

   for k in range(1,itmax+1):                            # loop of iterations
      err = 0e0
      for i in range(1,n+1):
         delta = t[i]                                            # correction
         for j in range(1,n+1): delta += s[i][j]*x[j]
         x[i] += delta                        # new approximation to solution
         if (x[i]): delta /= x[i]                            # relative error
         if (fabs(delta) > err): err = fabs(delta)            # maximum error

      if (err <= eps): break                              # check convergence

   if (k > itmax): printf("GaussSeidel: max. no. of iterations exceeded !")

   return err

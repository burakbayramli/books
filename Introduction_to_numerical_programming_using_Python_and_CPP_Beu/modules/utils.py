#--------------------------------- utils.py ---------------------------------
#  Contains utility routines.
#  Part of the numxlib numerics library. Author: Titus Beu, 2013
#----------------------------------------------------------------------------
from math import *

# Nearest integer
def Nint(x): return int(floor(x + 0.5))

# Transfers the sign of y to the absolute value of x
def Sign(x,y): return (abs(x) if y > 0 else -abs(x))

#============================================================================
def Magn(x):
#----------------------------------------------------------------------------
# Returns the order of magnitude of x as 10^n
#----------------------------------------------------------------------------
   return 0e0 if x == 0e0 else \
          10e0**int(log10(abs(x))) if abs(x) >= 1e0 else \
          0.1e0**(int(abs(log10(abs(x))))+1e0)

#============================================================================
def Index(x, ind, n):
#----------------------------------------------------------------------------
#  Ascending indexing of array x[1..n] in ind[] by insertion sort
#----------------------------------------------------------------------------
   for i in range(1,n+1): ind[i] = i                 # initialize index array

   for ipiv in range(2,n+1):                               # loop over pivots
      xpiv = x[ipiv]                        # save pivot to free its location
      i = ipiv - 1                               # initialize sublist counter
      while ((i > 0) and (x[ind[i]] > xpiv)):     # scan to the left of pivot
         ind[i+1] = ind[i]                 # item > pivot: shift to the right
         i -= 1

      ind[i+1] = ipiv                 # insert pivot into last freed location

#-------------------------------- random1.py --------------------------------
#  Contains routines for generating pseudo-random numbers.
#  Part of the numxlib numerics library. Author: Titus Beu, 2013
#----------------------------------------------------------------------------
from math import *
from random import *

# Generates random numbers with exponential distribution
def randExp(): return -log(1e0 - random())

#============================================================================
def randLCG1(iopt=1):
#----------------------------------------------------------------------------
#  Linear Congruential Generator for real random numbers in the range [0,1)
#  Press, Teukolsky, Vetterling, Flannery, Numerical Recipes 3rd Ed., 2007
#  iopt = 0 initializes the sequence
#----------------------------------------------------------------------------
   global irnd                                      # conserved between calls
   a = 8121; c = 28411; m = 134456

   if (iopt == 0): irnd = randrange(0xFFFFFFFF)              # initialization

   irnd = (a * irnd + c) % m
   return irnd/m

#============================================================================
def randLCG2(iopt=1):
#----------------------------------------------------------------------------
#  Linear Congruential Generator for real random numbers in the range [0,1)
#  D. Rapaport, The Art of Molecular Dynamics Simulation, Cambridge, 2004
#  iopt = 0 initializes the sequence
#----------------------------------------------------------------------------
   global irnd                                      # conserved between calls
   a = 314159269; c = 453806245; m = 2147483647

   if (iopt == 0): irnd = randrange(0xFFFFFFFF)              # initialization

   irnd = (a * irnd + c) & m
   return irnd/m

#============================================================================
def randMCG(iopt=1):
#----------------------------------------------------------------------------
#  Multiply-with-Carry Generator for real random numbers in the range [0,1)
#  George Marsaglia, post to Sci. Stat. Math, 1999
#  iopt = 0 initializes the sequence
#----------------------------------------------------------------------------
   global irnd1, irnd2                              # conserved between calls

   if (iopt == 0):                                           # initialization
      irnd1 = randrange(0xFFFFFFFF); irnd2 = randrange(0xFFFFFFFF)

   irnd1 = 36969 * (irnd1 & 0xFFFF) + (irnd1 >> 0xF)
   irnd2 = 18000 * (irnd2 & 0xFFFF) + (irnd2 >> 0xF)
   return (((irnd1 << 0xF) + irnd2) & 0xFFFFFFFF)/0xFFFFFFFF

#============================================================================
def randNrm():
#----------------------------------------------------------------------------
#  Returns random numbers with normal distribution 
#  w = exp(-0.5e0*x*x) / sqrt(2*pi)
#  using the central limit theorem and sums of 12 uniform random numbers
#----------------------------------------------------------------------------
   sum = 0e0
   for i in range(1,13): sum += random()
   x = sum - 6e0
   w = 0.398942280401433 * exp(-0.5e0*x*x)         # 1/sqrt(2*pi) = 0.3989...
   return (w, x)

#============================================================================
def randNrm2():
#----------------------------------------------------------------------------
#  Returns 2 random numbers (x,y) with normal distribution
#  w = exp(-(x*x+y*y)/2) / (2*pi)
#  and the corresponding distribution value 
#----------------------------------------------------------------------------
   r2 = -log(1e0 - random())            # exponential distribution for r**2/2
   w = exp(-r2) / (2e0*pi)                      # distribution function value
   r = sqrt(2e0*r2); theta = 2e0 * pi * random()          # polar coordinates
   x = r * cos(theta); y = r * sin(theta)             # Cartesian projections
   return (w, x, y)

#============================================================================
def randMet(w, delta, iopt=1):
#----------------------------------------------------------------------------
#  Generates random numbers x with the distribution w(x) by the Metropolis
#  method, using a maximal trial step size delta
#  iopt = 0 initializes the sequence
#----------------------------------------------------------------------------
   global xrnd                                      # conserved between calls

   if (iopt == 0): xrnd = 2e0*random() - 1e0                 # initialization

   dx = delta * (2e0*random() - 1e0)                        # trial step size
   if (random() <= w(xrnd+dx)/w(xrnd)): xrnd += dx   # accept with prob. w(x)

   return xrnd

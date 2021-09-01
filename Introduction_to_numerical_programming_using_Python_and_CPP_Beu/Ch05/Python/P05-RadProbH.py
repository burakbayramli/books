# Radial probability density in hydrogen atom
from math import *
from specfunc import *
from graphlib import *

#============================================================================
def Fact(n):
#----------------------------------------------------------------------------
#  Calculates the factorial of n
#----------------------------------------------------------------------------
   f = 1e0
   for i in range(2,n+1): f *= i
   return f

#============================================================================
def RadPsiH(n, l, r):
#----------------------------------------------------------------------------
#  Evaluates the radial wave function of the hydrogen atom for principal
#  quantum number n and orbital quantum number l at radius r (a0 is taken 1)
#----------------------------------------------------------------------------
   fNorm = sqrt(8e0/(n*n*n)*Fact(n-l-1)/(2e0*n*pow(Fact(n+l),3)))
     
   return fNorm * pow(r,l) * exp(-0.5e0*r) * aLaguerre(n+l,2*l+1,r)

# main

GraphInit(1200,800)

nmax = 3                                                 # maximum value of n
maxplotx = 3                                      # max. no. of plots along x

rmax = 50e0
h = 0.1e0                                                         # x spacing
nr = int(rmax/h) + 1                                          # no. of points

x = [0]*(nr+1)
y = [0]*(nr+1)

nplot = nmax*(nmax+1)/2                                  # total no. of plots
nplotx = min(nplot,maxplotx)                           # no. of plots along x
nploty = int(nplot/nplotx)                             # no. of plots along y
if (nplot % nplotx): nploty += 1                    # incomplete row of plots

dplotx = 1e0/nplotx                      # fractional width of a plot along x
dploty = 1e0/nploty                      # fractional width of a plot along y

xplot = 0; yplot = 0                              # lower-left corner of plot
for n in range(0,nmax+1):                                            # n-loop
   for l in range (0,n):                                             # l-loop
      for i in range(1,nr+1):                                        # r-loop
         r = (i-1) * h
         f = RadPsiH(n,l,r)
         x[i] = r
         y[i] = r * r * f * f
      
      fxmin = xplot + 0.20*dplotx; fxmax = xplot + 0.90*dplotx     # viewport
      fymin = yplot + 0.15*dploty; fymax = yplot + 0.85*dploty
      title = "r^2 |R_" + repr(n) + repr(l) + "(r)|^2"
      Plot(x,y,nr,"blue",1,fxmin,fxmax,fymin,fymax,"r","",title)
         
      xplot += dplotx
      if (xplot >= 1):                             # reached the right margin
         xplot = 0                                 # begin a new row of plots
         yplot += dploty

MainLoop()

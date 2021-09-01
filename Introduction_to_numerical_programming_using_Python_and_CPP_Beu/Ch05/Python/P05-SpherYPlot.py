# Plot squared spherical harmonics
from math import *
from specfunc import *
from graphlib import *

GraphInit(1200,1000)

lmax = 2                                                 # maximum value of l
maxplotx = 3                                      # max. no. of plots along x

h = pi/180                                              # theta angle spacing
n = int(2*pi/h) + 1                                     # no. of theta values

x = [0]*(n+1); y = [0]*(n+1)

nplot = 0
for l in range(0,lmax+1): nplot += (2*l+1)               # total no. of plots

nplotx = min(nplot,maxplotx)                           # no. of plots along x
nploty = int(nplot/nplotx)                             # no. of plots along y
if (nplot % nplotx): nploty += 1                    # incomplete row of plots

dplotx = 1e0/nplotx                      # fractional width of a plot along x
dploty = 1e0/nploty                      # fractional width of a plot along y

xplot = 0; yplot = 0                              # lower-left corner of plot
for l in range(0,lmax+1):                                            # l-loop
   for m in range (-l,l+1):                                          # m-loop
      for i in range(1,n+1):                                     # theta-loop
         theta = i * h
         (ReY,ImY) = SpherY(l,m,theta,0e0)               # spherical harmonic
         f = ReY * ReY + ImY * ImY                             # squared norm
         x[i] = f * sin(theta)                        # Cartesian projections
         y[i] = f * cos(theta)
      
      fxmin = xplot + 0.1*dplotx; fxmax = xplot + 0.9*dplotx       # viewport
      fymin = yplot + 0.1*dploty; fymax = yplot + 0.9*dploty
      title = "l = " + repr(l) + ",  m = " + repr(m)
      Plot(x,y,n,"blue",2,fxmin,fxmax,fymin,fymax,"","",title)
         
      xplot += dplotx
      if (xplot >= 1):                             # reached the right margin
         xplot = 0                                 # begin a new row of plots
         yplot += dploty

MainLoop()

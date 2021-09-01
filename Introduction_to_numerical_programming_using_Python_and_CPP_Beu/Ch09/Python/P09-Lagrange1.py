# Lagrange interpolation for a list of arguments
from modfunc import *
from graphlib import *

# main

nn  = [0]*4                                            # end indexes of plots
col = [""]*4                                                # colors of plots
sty = [0]*4                                                 # styles of plots

n  = 8                                                # number of data points
ni = 100                                     # number of interpolation points
n1 = n + ni; n2 = n + 2*ni                                      # end indexes

x  = [0]*(n +1); y  = [0]*(n +1)                                # data points
xi = [0]*(ni+1); yi = [0]*(ni+1)                       # interpolation points
xp = [0]*(n2+1); yp = [0]*(n2+1)                            # plotting arrays

x[1] = 0.15; x[2] = 0.2; x[3] = 0.3; x[4] = 0.5                # data points:
x[5] = 0.8 ; x[6] = 1.1; x[7] = 1.4; x[8] = 1.7                # f(x) = 1/x
for i in range(1,n+1): y[i] = 1e0/x[i]

h = (x[n]-x[1])/(ni-1)
for i in range(1,ni+1): xi[i] = x[1] + (i-1)*h      # interpolation arguments

Lagrange1(x,y,n,xi,yi,ni)

GraphInit(800,600)

for i in range(1,n+1):                              # fill in plotting arrays
   xp[i] = x[i]; yp[i] = y[i]                                   # data points
for i in range(1,ni+1):
   xp[n +i] = xi[i]; yp[n +i] = yi[i]                           # interpolant
   xp[n1+i] = xi[i]; yp[n1+i] = 1e0/xi[i]                 # original function

nn[1] = n ; col[1] = "red"  ; sty[1] =  0                       # data points
nn[2] = n1; col[2] = "blue" ; sty[2] =  1                       # interpolant
nn[3] = n2; col[3] = "black"; sty[3] = -1                 # original function
MultiPlot(xp,yp,yp,nn,col,sty,3,10,0e0,0e0,0,0e0,0e0,0,
          0.15,0.95,0.15,0.85,"x","P(x)","Lagrange interpolation")

MainLoop()

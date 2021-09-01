# Lagrange interpolation and Neville's algorithm
from modfunc import *
from graphlib import *

# main

nn  = [0]*4                                            # end indexes of plots
col = [""]*4                                                # colors of plots
sty = [0]*4                                                 # styles of plots

n  = 8                                                # number of data points
ni = 50                                      # number of interpolation points
n1 = n + ni; n2 = n + 2*ni                                      # end indexes

x  = [0]*(n +1); y  = [0]*(n +1)                                # data points
xp = [0]*(n2+1); yp = [0]*(n2+1)                            # plotting arrays
err = [0]*(n2+1)                                       # interpolation errors

x[1] = 0.15; x[2] = 0.2; x[3] = 0.3; x[4] = 0.5            # uneven abscissas
x[5] = 0.8 ; x[6] = 1.1; x[7] = 1.4; x[8] = 1.7
# h = (x[n]-x[1])/(n-1)
# for i in range(1,n+1): x[i] = x[1] + (i-1)*h     # equally spaced abscissas
for i in range(1,n+1):
   xp[i] = x[i]
   yp[i] = y[i] = 1e0/x[i]

GraphInit(1200,600)

#----------------------------------------------------- Lagrange interpolation
h = (x[n]-x[1])/(ni-1)
for i in range(1,ni+1):                             # fill in plotting arrays
   xi = x[1] + (i-1)*h                               # interpolation argument
   xp[n +i] = xi; yp[n +i] = Lagrange(x,y,n,xi)                 # interpolant
   xp[n1+i] = xi; yp[n1+i] = 1e0/xi                       # original function

nn[1] = n       ; col[1] = "red"  ; sty[1] =  0               # observed data
nn[2] = n +   ni; col[2] = "blue" ; sty[2] =  1                 # interpolant
nn[3] = n + 2*ni; col[3] = "black"; sty[3] = -1                       # model
MultiPlot(xp,yp,err,nn,col,sty,3,10,0e0,0e0,0,0e0,0e0,0,
          0.07,0.47,0.15,0.85,"x","P(x)","Lagrange interpolation")

#------------------------------------------------------ Neville interpolation
h = (x[n]-x[1])/(ni-1)
for i in range(1,ni+1):                             # fill in plotting arrays
   xi = x[1] + (i-1)*h                               # interpolation argument
   xp[n +i] = xi; yp[n +i],err[n+i] = Neville(x,y,n,xi)         # interpolant
   xp[n1+i] = xi; yp[n1+i] = 1e0/xi                       # original function

nn[1] = n       ; col[1] = "red"  ; sty[1] =  0               # observed data
nn[2] = n +   ni; col[2] = "blue" ; sty[2] = -4                 # interpolant
nn[3] = n + 2*ni; col[3] = "black"; sty[3] = -1                       # model
MultiPlot(xp,yp,err,nn,col,sty,3,10,0e0,0e0,0,0e0,0e0,0,
          0.57,0.97,0.15,0.85,"x","P(x)","Neville interpolation")

MainLoop()

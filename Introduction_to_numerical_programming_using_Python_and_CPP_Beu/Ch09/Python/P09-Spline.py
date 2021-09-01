# Interpolation with cubic splines
from modfunc import *
from graphlib import *

def sinc(x): return (sin(x)/x if x else 1e0)

# main

xmin = 0e0; xmax = 5*asin(1e0)              # tabulation interval: [0,5*pi/2]
n  = 6                                                # number of data points
ni = 100                                     # number of interpolation points
n1 = n + ni; n2 = n + 2*ni; n3 = n + 3*ni; n4 = n + 4*ni        # end indexes
nn  = [0]*6                                            # end indexes of plots
col = [""]*6                                                # colors of plots
sty = [0]*6                                                 # styles of plots

a = [0]*(n+1); b = [0]*(n+1)                            # spline coefficients
c = [0]*(n+1); d = [0]*(n+1)
x = [0]*(n+1); y = [0]*(n+1)                     # coordinates of data points
xp = [0]*(n4+1); yp = [0]*(n4+1)                            # plotting arrays

nn[1] = n ; col[1] = "black"; sty[1] =  0                       # data points
nn[2] = n1; col[2] = "black"; sty[2] = -1                 # original function
nn[3] = n2; col[3] = "blue" ; sty[3] =  1                       # interpolant
nn[4] = n3; col[4] = "red"  ; sty[4] = -1                    # 1st derivative
nn[5] = n4; col[5] = "green"; sty[5] = -1                    # 1st derivative

h = (xmax-xmin)/(n-1)                                  # generate data points
for i in range(1,n+1):
   xi = xmin + (i-1)*h
   xp[i] = x[i] = xi
   yp[i] = y[i] = sinc(xi)

h = (xmax-xmin)/(ni-1)
for i in range(1,ni+1):
   xi = xmin + (i-1)*h                              # interpolation arguments
   xp[n3+i] = xp[n2+i] = xp[n1+i] = xp[n+i] = xi         # plotting arguments

GraphInit(1200,600)

#------------------------------------------------------------ natural splines
Spline(x,y,n,0e0,0e0,0,a,b,c,d,x,y,0)

for i in range(1,ni+1):
   xi = xp[n+i]                                      # interpolation argument
   ip = 1
   while (ip < n-1 and xi > x[ip+1]): ip += 1               # index of spline

   yp[n +i] = sinc(xi)                                    # original function
   yp[n1+i] = ((a[ip]*xi + b[ip])*xi + c[ip])*xi + d[ip]             # spline
   yp[n2+i] = (3*a[ip]*xi + 2*b[ip])*xi + c[ip]              # 1st derivative
   yp[n3+i] = 6*a[ip]*xi + 2*b[ip]                           # 2nd derivative

MultiPlot(xp,yp,yp,nn,col,sty,5,10,0e0,0e0,0,0e0,0e0,0,0.07,0.47,0.15,0.85,
          "x","S","Spline interpolation - natural splines")

#------------------------------------------------------------ clamped splines
Spline(x,y,n,0e0,0e0,1,a,b,c,d,x,y,0)

for i in range(1,ni+1):
   xi = xp[n+i]                                      # interpolation argument
   ip = 1
   while (ip < n-1 and xi > x[ip+1]): ip += 1               # index of spline

   yp[n +i] = sinc(xi)                                    # original function
   yp[n1+i] = ((a[ip]*xi + b[ip])*xi + c[ip])*xi + d[ip]             # spline
   yp[n2+i] = (3*a[ip]*xi + 2*b[ip])*xi + c[ip]              # 1st derivative
   yp[n3+i] = 6*a[ip]*xi + 2*b[ip]                           # 2nd derivative

MultiPlot(xp,yp,yp,nn,col,sty,5,10,0e0,0e0,0,0e0,0e0,0,0.57,0.97,0.15,0.85,
          "x","S","Spline interpolation - clamped splines")

MainLoop()
